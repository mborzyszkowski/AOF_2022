package mb
package noSpaceLeftOnDevice

import noSpaceLeftOnDevice.FolderTree.{File, Folder}
import noSpaceLeftOnDevice.FolderTreeZipper.{EmptyZipper, FileZipper, FolderZipper}

import scala.annotation.tailrec
import scala.util.Try

trait FolderTreeZipper {

  def left: Seq[FolderTree]

  def up: Option[FolderTreeZipper]

  def right: Seq[FolderTree]

  def goDown: FolderTreeZipper

  def goUp: FolderTreeZipper

  def goLeft: FolderTreeZipper

  def goRight: FolderTreeZipper

  def insertLeft(folderTree: FolderTree): FolderTreeZipper

  def insertRight(folderTree: FolderTree): FolderTreeZipper

  def focus(name: String): FolderTreeZipper

  @tailrec
  final def getLeft(name: String): FolderTreeZipper =
    if (left.head.name == name) goLeft else goLeft.getLeft(name)

  @tailrec
  final def getRight(name: String): FolderTreeZipper =
    if (right.head.name == name) goRight else goRight.getRight(name)

  @tailrec
  final def toFolderTree: FolderTree =
    this match {
      case FolderZipper(name, children, _, up, _) if up.isEmpty => Folder(name, children)
      case FolderZipper(_, _, _, _, _) => goUp.toFolderTree
      case FileZipper(_, _, _, _, _) => goUp.toFolderTree
      case EmptyZipper(_, _, _) => goUp.toFolderTree
    }
}

object FolderTreeZipper {

  @tailrec
  def transform(zipper: FolderTreeZipper, commands: Seq[String]): FolderTreeZipper =
    commands match {
      case command :: anotherCommands => transform(processCommand(zipper, command), anotherCommands)
      case _ => zipper
    }

  private def processCommand(zipper: FolderTreeZipper, command: String): FolderTreeZipper =
    command.split(" ").toList match {
      case "$" :: "cd" :: ".." :: _ => zipper.goUp
      case "$" :: "cd" :: fileName :: _ => zipper.focus(fileName)
      case "$" :: "ls" :: _ => zipper.goDown
      case "dir" :: folderName :: _ => zipper.insertRight(Folder(folderName))
      case size :: name :: _ if Try(size.toInt).isSuccess => zipper.insertRight(File(name, size.toInt))
      case command => throw new Error(s"Unknown command: $command")
    }


  case class FolderZipper(name: String, children: Seq[FolderTree], left: Seq[FolderTree], up: Option[FolderTreeZipper], right: Seq[FolderTree]) extends FolderTreeZipper {

    override def goDown: FolderTreeZipper =
      if (children.isEmpty)
        EmptyZipper(this)
      else
        children.head match {
          case Folder(childName, childChildren) => FolderZipper(childName, childChildren, Seq(), Option(this), children.tail)
          case File(childName, childSize) => FileZipper(childName, childSize, Seq(), Option(this), children.tail)
        }

    override def goUp: FolderTreeZipper =
      up.get match {
        case FolderZipper(folderName, _, folderLeft, folderUp, folderRight) =>
          FolderZipper(folderName, (left :+ Folder(name, children)) ++ right, folderLeft, folderUp, folderRight)
      }

    override def goLeft: FolderTreeZipper =
      if (left.isEmpty)
        throw new Error(s"$name do not have left sibling")
      else
        left.head match {
          case Folder(folderName, folderChildren) => FolderZipper(folderName, folderChildren, left.tail, up, right :+ Folder(name, children))
          case File(fileName, fileSize) => FileZipper(fileName, fileSize, left.tail, up, right :+ Folder(name, children))
        }

    override def goRight: FolderTreeZipper =
      if (right.isEmpty)
        throw new Error(s"$name do not have right sibling")
      else
        right.head match {
          case Folder(folderName, folderChildren) => FolderZipper(folderName, folderChildren, left :+ Folder(name, children), up, right.tail)
          case File(fileName, fileSize) => FileZipper(fileName, fileSize, left :+ Folder(name, children), up, right.tail)
        }

    override def insertLeft(folderTree: FolderTree): FolderTreeZipper =
      FolderZipper(name, children, left :+ folderTree, up, right)

    override def insertRight(folderTree: FolderTree): FolderTreeZipper =
      FolderZipper(name, children, left, up, right :+ folderTree)

    override def focus(name: String): FolderTreeZipper = {
      if (left.exists(_.name == name))
        getLeft(name)
      else if (right.exists(_.name == name))
        getRight(name)
      else
        throw new Error("No sibling found")
    }
  }

  object FolderZipper {

    def apply(name: String): FolderZipper = apply(name, Seq())

    def apply(name: String, children: Seq[FolderTree]): FolderZipper = FolderZipper(name, children, Seq(), None, Seq())
  }


  case class FileZipper(name: String, size: Int, left: Seq[FolderTree], up: Option[FolderTreeZipper], right: Seq[FolderTree]) extends FolderTreeZipper {

    override def goDown: FolderTreeZipper =
      throw new Error(s"File($name) can not have children")

    override def goUp: FolderTreeZipper =
      up.get match {
        case FolderZipper(folderName, _, folderLeft, folderUp, folderRight) =>
          FolderZipper(folderName, (left :+ File(name, size)) ++ right, folderLeft, folderUp, folderRight)
      }

    override def goLeft: FolderTreeZipper =
      if (left.isEmpty)
        throw new Error(s"$name do not have left sibling")
      else
        left.head match {
          case Folder(folderName, folderChildren) => FolderZipper(folderName, folderChildren, left.tail, up, right :+ File(name, size))
          case File(fileName, fileSize) => FileZipper(fileName, fileSize, left.tail, up, right :+ File(name, size))
        }

    override def goRight: FolderTreeZipper =
      if (right.isEmpty)
        throw new Error(s"$name do not have right sibling")
      else
        right.head match {
          case Folder(folderName, folderChildren) => FolderZipper(folderName, folderChildren, left :+ File(name, size), up, right.tail)
          case File(fileName, fileSize) => FileZipper(fileName, fileSize, left :+ File(name, size), up, right.tail)
        }

    override def insertLeft(folderTree: FolderTree): FolderTreeZipper =
      FileZipper(name, size, left :+ folderTree, up, right)

    override def insertRight(folderTree: FolderTree): FolderTreeZipper =
      FileZipper(name, size, left, up, right :+ folderTree)

    override def focus(name: String): FolderTreeZipper = {
      if (left.exists(_.name == name))
        getLeft(name)
      else if (right.exists(_.name == name))
        getRight(name)
      else
        throw new Error("No sibling found")
    }
  }


  case class EmptyZipper(left: Seq[FolderTree], up: Option[FolderTreeZipper], right: Seq[FolderTree]) extends FolderTreeZipper {

    override def goDown: FolderTreeZipper =
      throw new Error(s"Empty can not have children")

    override def goUp: FolderTreeZipper =
      up.get match {
        case FolderZipper(folderName, _, folderLeft, folderUp, folderRight) =>
          FolderZipper(folderName, left ++ right, folderLeft, folderUp, folderRight)
      }

    override def goLeft: FolderTreeZipper =
      if (left.isEmpty)
        throw new Error(s"Empty do not have left sibling")
      else
        left.head match {
          case Folder(folderName, folderChildren) => FolderZipper(folderName, folderChildren, left.tail, up, right)
          case File(fileName, fileSize) => FileZipper(fileName, fileSize, left.tail, up, right)
        }

    override def goRight: FolderTreeZipper =
      if (right.isEmpty)
        throw new Error(s"Empty do not have right sibling")
      else
        right.head match {
          case Folder(folderName, folderChildren) => FolderZipper(folderName, folderChildren, left, up, right.tail)
          case File(fileName, fileSize) => FileZipper(fileName, fileSize, left, up, right.tail)
        }

    override def insertLeft(folderTree: FolderTree): FolderTreeZipper =
      EmptyZipper(left :+ folderTree, up, right)

    override def insertRight(folderTree: FolderTree): FolderTreeZipper =
      EmptyZipper(left, up, right :+ folderTree)

    override def focus(name: String): FolderTreeZipper = {
      if (left.exists(_.name == name))
        getLeft(name)
      else if (right.exists(_.name == name))
        getRight(name)
      else
        throw new Error("No sibling found")
    }
  }

  object EmptyZipper {
    def apply(up: FolderTreeZipper): EmptyZipper = new EmptyZipper(Seq(), Option(up), Seq())
  }
}
