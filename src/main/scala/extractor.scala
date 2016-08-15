package utils

import java.io.File
import java.io.IOException
import utils.Status._
import scala.sys.process._

object Extractor {
    
    /** Split a filepath into a path and a filename
     */
    def getPathAndFileName(filepath: String) = {
        val files = filepath.split('/')
        val filename = files(files.length - 1)
        val path = filepath.substring(0, filepath.length - filename.length)
        (path, filename)
    }

    /** Detect whether a file is an archive or note
      * using its extension (quick and dirty way, yes).
      */
    def isAnArchive(filename: String) = {
        if (
            filename.endsWith(".tar") ||
            filename.endsWith(".gz") ||
            filename.endsWith(".tgz") ||
            filename.endsWith(".zip") ||
            filename.endsWith(".bz2") ||
            filename.endsWith(".xz") ||
            filename.endsWith(".7z") ||
            filename.endsWith(".rar")
        )
            true
        else
            false
    }
    
    /** Create a directory path corresponding to an archive
      * by removing the extension from the archive name
      * and keeping the same path dsd.
      *
      * This function can be modified later to for example include
      * a random id in the directory name, if you want to make it
      * unique.
      */
    def getDirectoryPath(path: String, fileName: String) = {
        val directoryPath = if (path.isEmpty) "./" else path
        val directoryName = fileName.split('.')(0)
        directoryPath + directoryName
    }
    
    /** Make sure that the path ends with '/'
      */
    def completeDirectoryPath(path: String) = {
        if (path.endsWith("/")) path
        else path + "/"
    }
    
    def createDirectory(directoryPath: String): Status = {
        val directory = new File(directoryPath)
        if(directory.exists && !directory.isDirectory)
            return DIRECTORY_FILE_COLLISION.withArgument(directoryPath)
        
        try {
            directory.mkdir()
            return OK
        }
        catch {
            case se: SecurityException  => return SECURITY_ERROR.withArgument(se) 
            case ioe: IOException       => return IO_ERROR.withArgument(ioe)  
            case e: Exception           => return UNKNOWN_ERROR.withArgument(e)
        }
    }
    
    def executeCommand(cmd: String): (Status, String) = {
        val result = cmd.!!
        return (OK, result)
    }
    
    def extractLines(file: File) = {
        val source = scala.io.Source.fromFile(file.getPath)
        val lines = try source.mkString finally source.close()
        lines
    }
    def extractArchive(archivePath: String, extractionDirectory: String) = {
        val cmd = "tar -xvf " + archivePath + " -C " + extractionDirectory
        val (status, result) = executeCommand(cmd)
        if (status.isBad)
            status
        else
            OK
    }
    
    def extractArchiveRSync(archivePath: String, extractionDirectory: String) = {
        val cmd = "rsync -aHSx --no-devices --no-specials " + archivePath + " " + extractionDirectory
        val (status, result) = executeCommand(cmd)
        if (status.isBad)
            status
        else
            OK
    }
}

