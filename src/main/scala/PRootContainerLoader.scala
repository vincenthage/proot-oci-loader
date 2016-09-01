package runtime

import scala.io.Source
import java.io.File
import java.io.PrintWriter
import utils.Status._
import utils.Extractor._
import utils.OCI._
import utils.JSONUtils._

object PRootContainerLoader {

    val rootfsName = "rootfs"
    val scriptName = "launcher.sh"
    
    def main(args: Array[String]) {
        if (args.length == 0) {
            println(INVALID_ARGUMENT)
        }
        else {
            val status = loadImage(args(0))
            if (status.isBad) {
                println(status)
            }
            else {
                note("---> image loaded.")
            }
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
        
        // 3. We squash the image by merging the image layers
        note("- squashing image")
        squashImage(directoryPath, manifestData.Layers) match {
            case OK                         =>
            case badStatus                  => return badStatus
        }
        
        // 4. We prepare a bash script that will contain the required options for PRoot
        note("- preparing launcher script")
        generatePRootScript(directoryPath, Some(configData)) match {
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
    
    
    val standardVarsFuncName = "prepareStandardVars"
    val envFuncName = "prepareEnv"
    val printCommandsFuncName = "printCommands"
    val infoVolumesFuncName = "infoVolumes"
    val infoPortsFuncName = "infoPorts"
    val runPRootFuncName = "runPRoot"
    val bashVarPrefix = "LOADER_"
    val workdirBashVAR = bashVarPrefix + "WORKDIR"
    val entryPointBashVar = bashVarPrefix + "ENTRYPOINT"
    val cmdBashVar = bashVarPrefix + "CMD"
    
    def generatePRootScript(directory: String, configInit: Option[ConfigurationData]): Status = {
        val config = configInit match {
            case Some(conf) => conf
            case None       => {
                val (_, configData) = analyseImage(directory) match {
                    case (OK, Some(d1), Some(d2))   => (d1, d2)
                    case (badStatus, _, _)          => return badStatus
                }
                configData
            }
        }
        
        val scriptFile = new File(directory + "/" + scriptName)
        val script = new PrintWriter(scriptFile)
        val writeln = (s: String) => script.write(s + "\n")
        val writelnln = (s: String) => script.write(s + "\n\n")
        
        writelnln("#!/usr/bin/env bash")
        
        val workDir = config.WorkingDir match {
            case Some(s) if !s.isEmpty  => s
            case _                      => "/"
        }
        val entryPoint = config.Entrypoint match {
            case Some(list)             => assembleCommandParts(list)
            case _                      => ""
        }
        val cmd = config.Cmd match {
            case Some(list)             => assembleCommandParts(list)
            case _                      => ""
        }
        prepareVariables(List(
            workdirBashVAR      + "=" + workDir,
            entryPointBashVar   + "=" + entryPoint,
            cmdBashVar          + "=" + cmd
        ), standardVarsFuncName, writeln)
        
        prepareEnvVariables(config.Env, envFuncName, writeln)
        prepareMapInfo(config.Volumes, "Data volumes", infoVolumesFuncName, writeln)
        prepareMapInfo(config.ExposedPorts, "Exposed ports", infoPortsFuncName, writeln)
        preparePRootCommand(writeln)
        preparePrintCommands(writeln)
        prepareCLI(writeln)
        
        script.close
        scriptFile.setExecutable(true)
        
        return OK
    }
    
    //MYSQL_ROOT_PASSWORD=secret ./launcher.sh run ../../proot rootfs "-b ./data/mysql:/var/lib/mysql -b ./data/log:/var/log/mysql"
    // MYSQL_ROOT_PASSWORD=secret ./launcher.sh run ../../proot rootfs "-b ./data2/mysql:/var/lib/mysql -b ./data2/log:/var/log/mysql -b ./data2/mysqld:/var/run/mysqld -p 3306:3308"
    
    def prepareCLI(write: String => Unit) {
        write("if (( \"$#\" < 1 )); then")
            write("\t" + printCommandsFuncName)
        write("elif [ \"$1\" = 'info' ]; then")
            write("\tif [ \"$2\" = 'volumes' ]; then")
                write("\t\t" + infoVolumesFuncName)
            write("\telif [ \"$2\" = 'ports' ]; then")
                write("\t\t" + infoPortsFuncName)
            write("\telse")
                write("\t\t" + infoVolumesFuncName)
                write("\t\t" + infoPortsFuncName)
            write("\tfi")
        
        write("elif [ \"$1\" = 'run' ]; then")
            write("\tif (( \"$#\" < 3 )); then")
                write("\t\t" + printCommandsFuncName)
            write("else")
                write("\t\t" + runPRootFuncName + " $2 $3 \"$4\" ${@:5} ")
            write("\tfi")
            
        write("else")
            write("\t" + printCommandsFuncName)
        write("fi\n")
    }
        
    def preparePRootCommand(write: String => Unit) {
        write("function " + runPRootFuncName + " {")
        write("\t" + envFuncName) // setting environment variables
        write("\t" + standardVarsFuncName) // setting standard proot variables
        write("\t" + assembleCommandParts(
            "$1", // calling PRoot
            "-R $2", // setting guest rootfs
            "-w $" + workdirBashVAR, // setting working directory
            "$3", // user additional PRoot options
            "$" + entryPointBashVar,
            "$" + cmdBashVar,
            "${@:4}" // user inputs for the program
        ))
        write("}\n")
    }
    
    def prepareVariables(args: List[String], functionName: String, write: String => Unit) {
        write("function " + functionName + " {")
        for(arg <- args)
            write("\t" + addQuoteToRightSideOfEquality(arg))
        write("}\n")
    }
        
    def prepareEnvVariables(maybeArgs: Option[List[String]], functionName: String, write: String => Unit) {
        write("function " + functionName + " {")
        maybeArgs match {
            case Some(args)     => {
                for(arg <- args)
                    write("\texport " + addQuoteToRightSideOfEquality(arg))
            }
            case _              =>
        }
        write("}\n")
    }
        
    def prepareMapInfo(maybeMap: Option[Map[String, EmptyObject]], title: String, functionName: String, write: String => Unit) {
        write("function " + functionName + " {")
        write("\techo \"" + title + ":\"")
        maybeMap match {
            case Some(map)      => {
                for((variable, _) <- map)
                    write("\techo \"\t" + variable + "\"")
                }
            case _              =>
        }
        write("}\n")
    }
    
    def preparePrintCommands(write: String => Unit) {
        write("function " + printCommandsFuncName + " {")
            write("\techo 'Commands:'")
            write("\techo '\tinfo <arg1>'")
            write("\techo '\t\tDisplay the data volumes or ports used the image.'")
            write("\techo '\t\t<arg1>: nothing, volumes or ports'") 
            write("\techo ''") 
            write("\techo '\trun <arg1> <arg2> <arg3> <args...>'")
            write("\techo '\t\tRun the image using PRoot.'")
            write("\techo '\t\t<arg1>: \tpath to the PRoot binary'")
            write("\techo '\t\t<arg2>: \tpath to the image root filesystem (rootfs)'")
            write("\techo '\t\t<arg3>: \toptions for PRoot. See PRoot manual for more info.'")
            write("\techo '\t\t\tWrap all of them with quotes (ex: \"-p 5432:5433 -p 8000:8001\").'")
            write("\techo '\t\t<args...>: \targuments for the image program'")
        write("}\n")
    }
    
    
    def assembleCommandParts(args:String*) = {
        var command = ""
        for (arg <- args)
            command += arg + " "
        command
    }
    
    def assembleCommandParts(args: List[String]) = {
        var command = ""
        for (arg <- args)
            command += arg + " "
        command
    }
    
    def addQuoteToRightSideOfEquality(s: String) = {
        s.split('=') match {
            case Array(left, right)  => left + "=\"" + right + "\""
            case _              => s
        }
    }
}
