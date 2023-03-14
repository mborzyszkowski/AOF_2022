package mb
package noSpaceLeftOnDevice

import noSpaceLeftOnDevice.FolderTree.{File, Folder}
import noSpaceLeftOnDevice.FolderTreeZipper.FolderZipper

object NoSpaceLeftOnDevice extends App {

  val filePath = "src/main/scala/noSpaceLeftOnDevice/input.txt"

  val folderTree = FolderTree.parseFromFile(filePath)

  val foldersWithTotalSizeNoMoreThanHundredThousand = folderTree.allFolders.filter(_.totalSize <= 100000)

  println(folderTree)
  println(foldersWithTotalSizeNoMoreThanHundredThousand.map(_.name))
  println(foldersWithTotalSizeNoMoreThanHundredThousand.map(_.totalSize).sum)

  println()
  println("----------------------------------------------------------")
  println()

  val totalDiskSpace = 70000000
  val diskSpaceNeededForUpdate = 30000000
  val occupiedDiskSpace = folderTree.totalSize
  val neededDiskSpaceToFree = diskSpaceNeededForUpdate + occupiedDiskSpace - totalDiskSpace

  val possibleFoldersToDelete =
    folderTree.allFolders
      .map(folder => (folder, folder.totalSize))
      .filter(_._2 >= neededDiskSpaceToFree)
      .sortBy(_._2)

  println(s"Possible folders to delete: ${possibleFoldersToDelete.map { case (folder, totalSize) => s"${folder.name}($totalSize)" }.mkString(" ")}")


  println(createExampleTree)

  def createExampleTree: FolderTree =
    FolderZipper("/")
      //$ cd /
      //$ ls
      //dir a
      //14848514 b.txt
      //8504156 c.dat
      //dir d
      .goDown
      .insertRight(Folder("a"))
      .insertRight(File("b.txt", 14848514))
      .insertRight(File("c.dat", 8504156))
      .insertRight(Folder("d"))
      //$ cd a
      //$ ls
      //dir e
      //29116 f
      //2557 g
      //62596 h.lst
      .focus("a")
      .goDown
      .insertRight(Folder("e"))
      .insertRight(File("f", 29116))
      .insertRight(File("g", 2557))
      .insertRight(File("h.lst", 62596))
      //$ cd e
      //$ ls
      //584 i
      //$ cd ..
      //$ cd ..
      .focus("e")
      .goDown
      .insertRight(File("i", 584))
      .goUp
      .goUp
      //$ cd d
      //$ ls
      //4060174 j
      //8033020 d.log
      //5626152 d.ext
      //7214296 k
      .focus("d")
      .goDown
      .insertRight(File("j", 4060174))
      .insertRight(File("d.log", 8033020))
      .insertRight(File("d.ext", 5626152))
      .insertRight(File("k", 7214296))
      .toFolderTree
}
