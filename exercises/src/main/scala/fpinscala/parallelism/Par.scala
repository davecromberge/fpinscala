package fpinscala.parallelism

import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) 
  
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get 
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }

  private case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
    var cache: Option[C] = None
    def isDone = cache.isDefined
    def get(timeout: Long, units: TimeUnit) = 
      compute(TimeUnit.MILLISECONDS.convert(timeout, units))
    def get = compute(Long.MaxValue)
    def isCancelled = a.isCancelled || b.isCancelled
    def cancel(evenIfRunning: Boolean): Boolean = 
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

    private def compute(timeoutMs: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.currentTimeMillis
        val ar = a.get(timeoutMs, TimeUnit.MILLISECONDS)
        val runTime = System.currentTimeMillis - start
        val br = b.get(timeoutMs - runTime, TimeUnit.MILLISECONDS)
        cache = Some(f(ar, br))
        cache.get
    }
  }
  
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = 
    (es: ExecutorService) => 
      Map2Future(a(es), b(es), f)
  
  def fork[A](a: => Par[A]): Par[A] = 
    es => es.submit(new Callable[A] { 
      def call = a(es).get
    })

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => 
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = 
    if (ints.size <= 1)
      ints.headOption getOrElse 0 
    else { 
      val (l,r) = ints.splitAt(ints.length/2) 
      sum(l) + sum(r) 
    }

}
