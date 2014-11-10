package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case object Empty extends Tree[Nothing]


object Tree {
  
  def size[A](tree: Tree[A]): Int =
    Tree.fold(tree, 0)((x, y) => 1)(_ + _)

  def maximum(tree: Tree[Int]): Int = 
    Tree.fold(tree, 0)(_ max _)(_ max _)

  def depth[A](tree: Tree[A]): Int = 
    Tree.fold(tree, 0)((x, y) => y + 1)(_ + _)

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = 
    Tree.fold(tree, Empty: Tree[B])((x, y) => Leaf(f(x)))((a, b) => Branch(a, b))

  def fold[A, B](tree: Tree[A], z: B)(f: (A, B) => B)(combine: (B, B) => B): B =
    tree match {
      case Empty => z
      case Leaf(a) => f(a, z)
      case Branch(a, b) => 
        combine(Tree.fold(a, z)(f)(combine), Tree.fold(b, z)(f)(combine))
    }



}
