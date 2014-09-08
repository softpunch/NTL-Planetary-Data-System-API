# Script to run LROC import command line tool

# Prepare class path
source ./export_classpath.sh

# Invoke the main method
java gov.nasa.pds.commandline.LROCImportCommandLine $1 $2 $3