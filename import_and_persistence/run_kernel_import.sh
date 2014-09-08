# Script to run kernel import command line tool

# Prepare class path
source ./export_classpath.sh

# Invoke the main method
java gov.nasa.pds.commandline.KernelImportCommandLine $1
