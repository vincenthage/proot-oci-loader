import org.scalatest._
import utils.Extractor._


class UtilsSpec extends FlatSpec with Matchers {

  "Extractor" should "split a file path in its path and filename" in {
      val filepath = "/home/tester/workspace/proot-container-loader/images/mysql.tar.gz"
      val (path, filename) = getPathAndFileName(filepath)
      path should equal("/home/tester/workspace/proot-container-loader/images/")
      filename should equal("mysql.tar.gz")
  }

  it should "split a local filepath into an empty path and the filename" in {
      val filepath = "mysql.tar.gz"
      val (path, filename) = getPathAndFileName(filepath)
      path should equal("")
      filename should equal("mysql.tar.gz")
  }
  
  it should "detect that *.tar, *.tar.gz, *.zip and *.rar files are archives" in {
      isAnArchive("mysql.tar.gz") should equal(true)
      isAnArchive("mysql.tar") should equal(true)
      isAnArchive("mysql.rar") should equal(true)
      isAnArchive("mysql.zip") should equal(true)
  }
  
  it should "detect that other files are not archives" in {
      isAnArchive("mysql") should equal(false)
      isAnArchive("mysql.txt") should equal(false)
      isAnArchive("mysql.json") should equal(false)
  }
  
  it should "construct a valid extraction directory path from a path and archive name" in {
      val path = "/home/tester/workspace/proot-container-loader/images/"
      val archiveName = "mysql.tar.gz"
      getDirectoryPath(path, archiveName) should equal("/home/tester/workspace/proot-container-loader/images/mysql")
  }
  
  it should "construct a valid extraction directory path from an empty path and archive name" in {
      val path = ""
      val archiveName = "mysql.tar.gz"
      getDirectoryPath(path, archiveName) should equal("./mysql")
  }
  

}