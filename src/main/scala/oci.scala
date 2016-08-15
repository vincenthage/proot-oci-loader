package utils

import utils.Status._
import net.liftweb.json._
import java.io.IOException
import java.nio.file._
import java.util.function.Predicate
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

object InternetProtocol {
    sealed abstract class InternetProtocol (val typeProtocol: String)
    case object TCP extends InternetProtocol("TCP")
    case object UDP extends InternetProtocol("UDP")
    case object OTHER extends InternetProtocol("OTHER")
}
import InternetProtocol._

object OCI {
    implicit val formats = DefaultFormats

    case class EmptyObject()
    case class ManifestData(
        Config: String, 
        Layers: List[String], 
        RepoTags: List[String]
        )
    case class ConfigurationData(
        Cmd: List[String], 
        Entrypoint: List[String], 
        Env: List[String],
        ExposedPorts: Map[String, EmptyObject],
        User: String,
        Volumes: Map[String, EmptyObject],
        WorkingDir: String
        )
    
    //TODO: test
    def getManifestFile(rootFiles: Array[String]): Option[String] = {
        val manifests = rootFiles.filter(_.endsWith("manifest.json"))
        if (!manifests.isEmpty) Some(manifests(0))
        else None
    }
    
    def harvestManifestData(manifestRaw: String) = {
        val rootAST = parse(manifestRaw)(0)
        rootAST.extract[ManifestData]
    }
    
    def harvestConfigData(configRaw: String) = {
        val rootAST = parse(configRaw)
        var configAST = rootAST \ "config"
        configAST.extract[ConfigurationData]
    }
    
    
    object WhiteoutUtils{
        /* @see <a href="https://github.com/docker/docker/blob/master/pkg/archive/whiteouts.go">Whiteout files</a> */
        /* indicates a file removed in recent layers */
        val whiteoutPrefix = ".wh."
        /* indicates special file, generally excluded from exported archives */
        val whiteoutMetaPrefix = whiteoutPrefix + whiteoutPrefix
        /* indicates a link removed in recent layers */
        val whiteoutLinkDir = whiteoutPrefix + "plnk"
        /* indicates that the current directory is inaccessible in recent layers 
        * (the directory file should have a whiteout file in the parent folder) */
        val whiteoutOpaqueDir = whiteoutPrefix + ".opq."
    
        def getPrefix(path: Path) = {
            var filename = path.getFileName.normalize().toString()
        
            if (filename.startsWith(whiteoutOpaqueDir)) whiteoutOpaqueDir
            else if (filename.startsWith(whiteoutLinkDir)) whiteoutLinkDir
            else if (filename.startsWith(whiteoutMetaPrefix)) whiteoutMetaPrefix
            else if (filename.startsWith(whiteoutPrefix)) whiteoutPrefix
            else ""
        }
    }
    import WhiteoutUtils._
    
    def removeWhiteouts(directoryPath: String): Status = {
        var whiteoutsBuffer = new ListBuffer[Path]()
    
        Files.walk(Paths.get(directoryPath)).iterator()
                .filter(_.getFileName.normalize().toString().startsWith(whiteoutPrefix))
                .foreach(
                    whiteoutPath => {
                        whiteoutsBuffer += whiteoutPath
                        
                        var prefix = getPrefix(whiteoutPath)
                        getConcernedFile(whiteoutPath, prefix) match {
                            case Some(path) => whiteoutsBuffer += path
                            case None       =>
                        }
                    }
                )
                
        val whiteouts = whiteoutsBuffer.toList
        whiteouts.foreach(path => deleteRecursively(path))
        
        return OK
    }
    
    def getConcernedFile(whiteoutFilePath: Path, prefix: String) = {
        val tempStr = whiteoutFilePath.normalize().toString()
        val filePathString = tempStr.replaceAll(prefix, "")
        val filePath = Paths.get(filePathString)
        
        Files.exists(filePath) match {
            case true   => Some(filePath)
            case false  => None
        }
    }
    
    def removeWhiteoutFileAuxiliary(whiteoutFilePath: Path, prefix: String): Status = {
        val pathString = whiteoutFilePath.normalize().toString()
        val correspondingFile = pathString.replaceAll(prefix, "")
        
        try {
            deleteRecursively(Paths.get(correspondingFile))
            deleteRecursively(Paths.get(pathString))
        }
        catch {
            case se: SecurityException  => return SECURITY_ERROR.withArgument(se) 
            case ioe: IOException       => return IO_ERROR.withArgument(ioe)  
            case dnee: DirectoryNotEmptyException => return IO_ERROR.withArgument(dnee)
            case e: Exception           => return UNKNOWN_ERROR.withArgument(e)
        }
        return OK
    }
    
    def deleteRecursively(path: Path) {
        if(Files.isDirectory(path)) {
            Files.walk(path).iterator()
                    .foreach(p => if(!p.equals(path)) deleteRecursively(p))
            note("---- Deleting dir " + path)
            Files.deleteIfExists(path)
        }
        else {
            note("---- Deleting file " + path)
            Files.deleteIfExists(path)
            }
    }
}


