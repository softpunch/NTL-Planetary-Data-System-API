/*
 * Copyright (C) 2014 TopCoder Inc., All Rights Reserved.
 */
package gov.nasa.pds.commandline;

import gov.nasa.pds.entities.MetadataObject;
import gov.nasa.pds.processors.LROCImportProcessor;
import gov.nasa.pds.services.DataSetProcessingException;

import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.FileSystemXmlApplicationContext;

/**
 * <p>
 * The {@link LROCImportCommandLine} class is used to import LROC. It provides an entry method for command line calling.
 * The required utility's parameter is the filename of the Spring configuration file.
 * </p>
 *
 * <strong>Thread Safety:</strong> This class is immutable and thread safe.
 *
 * <p>
 * Version 1.1 changes [NASA PDS API - LROC Update]:
 * <ol>
 * <li>add "product type" and "camera type" command line args</li>
 * </ol>
 * </p>
 *
 * 
 * @author fivestarwy, caoweiquan322
 * @version 1.1
 */
public class LROCImportCommandLine {
    /**
     * Private empty constructor.
     */
    private LROCImportCommandLine() {
    }

    /**
     * <p>
     * The main method.
     * </p>
     * <p>
     * Usage of this command line tool:
     * </p>
     * <p>
     * Run './run_lroc_import.sh <configuration_xml_filename> <product_type> <camera_type>' under root folder of this
     * project.
     * </p>
     * <p>
     * Ensure *.sh under root folder of this project is executable by running:
     * </p>
     * <p>
     * 'chmod +x *.sh'
     * </p>
     *
     * @param args
     *            the command line arguments.
     *
     * @throws BeansException
     *             in case of loading/parsing errors of configuration file or if the bean could not be found
     * @throws DataSetProcessingException
     *             if there is an error executing the engine
     * @throws ClassCastException
     *             if the processor bean is not of corresponding type
     */
    public static void main(String[] args) throws DataSetProcessingException {
        if (args.length < 3) {
            System.out
                    .print("Usage: './run_lroc_import.sh <configuration_xml_filename> <product_type> <camera_type>' ");
            System.out.println("under root folder of this project.");
            System.out.println("Ensure *.sh under root folder of this project is executable by running:");
            System.out.println("'chmod +x *.sh'");
            return;
        }
        ApplicationContext appContext = new FileSystemXmlApplicationContext(args[0]);
        LROCImportProcessor processor = (LROCImportProcessor) appContext.getBean("LROCImportProcessor");
        processor.importMapData(args[1], args[2]);
    }
}
