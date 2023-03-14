package mb
package noSpaceLeftOnDevice

import SourceWrapper.withSource
import noSpaceLeftOnDevice.FolderTree.Folder
import noSpaceLeftOnDevice.FolderTreeZipper.FolderZipper

sealed trait FolderTree {

  def name: String

  def allFolders: Seq[Folder]

  def totalSize: Int
}

object FolderTree {

  case class Folder(name: String, children: Seq[FolderTree]) extends FolderTree {

    override def allFolders: Seq[Folder] = {
      subFolders(this) :+ this
    }

    def subFolders(folder: Folder): Seq[Folder] =
      folder.children.flatMap(_.allFolders)

    override def totalSize: Int =
      children.map(_.totalSize).sum
  }

  object Folder {
    def apply(name: String): Folder = Folder(name, Seq())
  }


  case class File(name: String, size: Int) extends FolderTree {

    override def allFolders: Seq[Folder] = Seq()

    override def totalSize: Int = size
  }

  def parseFromFile(filePath: String): FolderTree = {
    val commands = withSource(filePath)(_.getLines().toSeq).tail

    FolderTreeZipper.transform(FolderZipper("/"), commands).toFolderTree
  }
}
