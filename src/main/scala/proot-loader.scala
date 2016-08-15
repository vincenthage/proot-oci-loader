package runtime

import scala.io.Source
import java.io.File
import utils.Status._
import utils.Extractor._
import utils.OCI._
import utils.JSONUtils._

object PRootContainerLoader {

    def main(args: Array[String]) {
        if (args.length == 0) {
            println(INVALID_ARGUMENT)
        }
        else {
            val status = loadImage(args(0))
            if (status.isBad)
                println(status)
            else
                note("---> image loaded.")
        }
    }
    
    
    def loadImage(imagePath: String): Status = {
        // 1. We prepare the image by extracting it
        note("- preparing image")
        val directoryPath = prepareImage(imagePath) match {
            case (OK, Some(path))                 => path
            case (badStatus, _)             => return badStatus
        }
        
        // 2. We explore the directory and start retrieving some info
        note("- analyzing image")
        val (manifestData, configData) = analyseImage(directoryPath) match {
            case (OK, Some(d1), Some(d2))   => (d1, d2)
            case (badStatus, _, _)          => return badStatus
        }
        
        // 3. We merge
        note("- analyzing image")
        squashImage(directoryPath, manifestData.Layers) match {
            case OK                         =>
            case badStatus                  => return badStatus
        }
        
        return OK
    }


    /** Check that the image or directory exists, and extract it if it hasn't been already.
      * Return the status and the directory where the image has been extracted.
      */
    def prepareImage(imagePath: String): (Status, Option[String]) = {
        var status: Status = OK
        val file = new File(imagePath)
        
        // first, we check that the path leads to somewhere
        if (!file.exists) return (INVALID_PATH.withArgument(imagePath), None)
           
        // then, either we're given an archive, or a directory
        if (!file.isDirectory) {
            val (path, archiveName) = getPathAndFileName(imagePath)
            if(!isAnArchive(archiveName)) 
                return (INVALID_IMAGE.withArgument(archiveName + " is not a valid archive file."), None)
            
            val directoryPath = getDirectoryPath(path, archiveName) 
            status = createDirectory(directoryPath)
            if (status.isBad) 
                return (status, None)
             
            note("-- extracting archive")
            status = extractArchive(path + archiveName, directoryPath)
            if (status.isBad) 
                return (status, None)
            
            return (OK, Some(completeDirectoryPath(directoryPath)))
        }
        else {
            note("-- already extracted")
            return (OK, Some(completeDirectoryPath(imagePath)))
        }
    }
    
    
    /** Retrieve metadata (layer ids, env variables, volumes, ports, commands)
      * from the manifest and configuration files.
      */
    def analyseImage(directoryPath: String): (Status, Option[ManifestData], Option[ConfigurationData]) = {
        val rootDirectory = new File(directoryPath)
    
        assert(rootDirectory.exists)
        
        note("-- extracting manifest data")
        
        val manifest = getManifestFile(rootDirectory.list) match {
            case None       => return (INVALID_IMAGE_FORMAT.withArgument("No manifest.json file in the root directory."), None, None)
            case Some(path) => new File(directoryPath + path)
        }
        
        val manifestLines = extractLines(manifest)
        val manifestData = harvestManifestData(manifestLines)
        note("manifestData: " + manifestData)
        
        val config = new File(directoryPath + manifestData.Config)
        if (!config.exists) return (INVALID_IMAGE_FORMAT.withArgument("No configuration json file in the root directory."), None, None)
        
        note("-- extracting configuration data")
        
        val configLines = extractLines(config)
        val configData = harvestConfigData(configLines)
        
        return (OK, Some(manifestData), Some(configData))
    }
    
    val rootfsName = "rootfs"
    
    /** Merge the layers by extracting them in order in a same directory.
      * Also, delete the whiteout files.
      */
    def squashImage(directoryPath: String, layers: List[String]): Status = {
        val rootfsPath = directoryPath + rootfsName
        createDirectory(rootfsPath) match {
            case OK         => 
            case status     => return status
        }
        
        layers.foreach{
            layerName => {
                note("-- extracting layer " + layerName)
                extractArchive(directoryPath + layerName, rootfsPath) match {
                    case OK         => 
                    case status     => return status
                }                
             }
        }
        
        note("-- removing whiteouts")
        removeWhiteouts(rootfsPath) match {
            case OK         => 
            case status     => return status
        }
        
        return OK
    }
    
}
