/*
 * Copyright (C) 2014 TopCoder Inc., All Rights Reserved.
 */
package gov.nasa.pds.commandline;

import gov.nasa.pds.processors.KernelImportProcessor;
import gov.nasa.pds.services.DataSetProcessingException;

import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.FileSystemXmlApplicationContext;

/**
 * <p>
 * The {@link KernelImportCommandLine} class is used to import kernels. It provides an entry method
 * for command line calling. The required utility's parameter is the filename of the Spring configuration
 * file.
 * </p>
 *
 * <strong>Thread Safety:</strong> This class is immutable and thread safe.
 *
 * @author fivestarwy, caoweiquan322
 * @version 1.0
 */
public class KernelImportCommandLine {
    /**
     * Private empty constructor.
     */
    private KernelImportCommandLine() {
    }

    /**
     * <p>
     * The main method.
     * </p>
     * <p>
     * Usage of this command line tool:
     * </p>
     * <p>
     * Run './run_kernel_import.sh <configuration_xml_filename>' under root folder of this project.
     * </p>
     * <p>
     * Ensure *.sh under root folder of this project is executable by running:
     * </p>
     * <p>
     * 'chmod +x *.sh'
     * </p>
     *
     * @param args
     *             the command line arguments.
     *
     * @throws BeansException
     *             in case of loading/parsing errors of configuration file or if the bean could not be found
     * @throws DataSetProcessingException
     *             if there is an error executing the engine
     * @throws ClassCastException
     *             if the processor bean is not of corresponding type
     */
    public static void main(String[] args) throws DataSetProcessingException {
        if (args.length < 1) {
            System.out.print("Usage: './run_kernel_import.sh <configuration_xml_filename>' ");
            System.out.println("under root folder of this project.");
            System.out.println("Ensure *.sh under root folder of this project is executable by running:");
            System.out.println("'chmod +x *.sh'");
            return;
        }
        ApplicationContext appContext = new FileSystemXmlApplicationContext(args[0]);
        KernelImportProcessor processor =
                (KernelImportProcessor) appContext.getBean("KernelImportProcessor");
        processor.importKernels();
    }
}

