/*
 * Copyright (C) 2014 TopCoder Inc., All Rights Reserved.
 */
package gov.nasa.pds.processors.impl;

import org.springframework.beans.factory.InitializingBean;

import com.topcoder.commons.utils.LoggingWrapperUtility;
import com.topcoder.commons.utils.ValidationUtility;
import com.topcoder.util.log.Log;

import gov.nasa.pds.processors.KernelImportProcessor;
import gov.nasa.pds.services.DataSetProcessingConfigurationException;
import gov.nasa.pds.services.DataSetProcessingException;

/**
 * <p>
 * The {@link KernelImportProcessorImpl} class implements {@link KernelImportProcessor} interface by providing
 * implementation of <code>importKernels()</code> method.
 * </p>
 * <p>
 * This class will import all Core kernel programs and LRO-specific kernels to a file system in some configurable
 * folder. The class uses rsync tool to download data from configured server. we can assume there is always one
 * instance running so there is no concurrency issue.
 * </p>
 *
 * <strong>Thread Safety:</strong> This class is not thread safe but will be used in thread safe manner.
 *
 * @author fivestarwy, caoweiquan322
 * @version 1.0
 */
public class KernelImportProcessorImpl implements KernelImportProcessor, InitializingBean {
    /**
     * Constant for the class name of this class. Used for logging.
     */
    private static final String CLASS_NAME = KernelImportProcessorImpl.class.getName();

    /**
     * <p>
     * The <code>Log</code> instance used for logging.
     * </p>
     *
     * <p>
     * It is initialized with Spring setter dependency injection. Cannot be <code>null</code> after initialization,
     * assuming that property is initialized via Spring setter-based dependency injection and is never changed
     * after that. Has a setter.
     * </p>
     */
    private Log logger;

    /**
     * <p>
     * The directory which used to keep the kernel in file system.
     * </p>
     *
     * <p>
     * It is initialized with Spring setter dependency injection. Cannot be <code>null</code> after initialization,
     * assuming that property is initialized via Spring setter-based dependency injection and is never changed
     * after that. Has a setter.
     * </p>
     */
    private String saveDirectory;

    /**
     * <p>
     * The name of rsync server. As per http://isis.astrogeology.usgs.gov/documents/InstallGuide/index.html.
     * </p>
     * <p>
     * The value should be "isisdist.astrogeology.usgs.gov" or "isisdist.wr.usgs.gov".
     * </p>
     *
     * <p>
     * It is initialized with Spring setter dependency injection. Cannot be <code>null</code> after initialization,
     * assuming that property is initialized via Spring setter-based dependency injection and is never changed
     * after that. Has a setter.
     * </p>
     */
    private String serverName;

    /**
     * Creates an instance of {@link KernelImportProcessorImpl}.
     */
    public KernelImportProcessorImpl() {
    }

    /**
     * <p>
     * Retrieve all Core kernel programs and LRO-specific kernels (LRO-specific kernels and store the kernels plainly
     * in a file system in some configurable folder.
     * </p>
     *
     * @throws DataSetProcessingException
     *             if there is any error while importing kernels.
     */
    public void importKernels() throws DataSetProcessingException {
        final String signature = CLASS_NAME + ".importKernels()";
        LoggingWrapperUtility.logEntrance(logger, signature,
                null, null);

        try {
            //Downloading the ISIS Binaries
            String cmd = "rsync -azv --delete --partial " + serverName + "::x86-64_linux_RHEL6/isis " + saveDirectory;
            Helper.executeCommand(cmd);

            //Download ISIS 3 Base Data
            cmd="rsync -azv --delete --partial " + serverName + "::isis3data/data/base " + saveDirectory + "/data/";
            Helper.executeCommand(cmd);

            //Download Lunar Reconnaissance Orbiter Mission (kernels can be included):
            cmd="rsync -azv --delete --partial " + serverName + "::isis3data/data/lro " + saveDirectory + "/data/";
            Helper.executeCommand(cmd);

            LoggingWrapperUtility.logExit(logger, signature, null);
        } catch (DataSetProcessingException e) {
            throw LoggingWrapperUtility.logException(logger, signature, e);
        }

    }

    /**
     * <p>
     * Check whether this class was initialized by spring properly.
     * </p>
     * <p>
     * Required parameters:
     * <ul>
     * <li>logger</li>
     * <li>saveDirectory</li>
     * <li>serverName</li>
     * </ul>
     * </p>
     *
     * @throws DataSetProcessingConfigurationException
     *             if logger is null or saveDirectory/serverName is null or empty.
     */
    @Override
    public void afterPropertiesSet() {
        ValidationUtility.checkNotNull(logger, "logger",
                DataSetProcessingConfigurationException.class);
        ValidationUtility.checkNotNullNorEmptyAfterTrimming(saveDirectory, "saveDirectory",
                DataSetProcessingConfigurationException.class);
        ValidationUtility.checkNotNullNorEmptyAfterTrimming(serverName, "serverName",
                DataSetProcessingConfigurationException.class);
        if (!serverName.equals("isisdist.astrogeology.usgs.gov")
                && !serverName.equals("isisdist.wr.usgs.gov")) {
            throw new DataSetProcessingConfigurationException("The property serverName must equal to "
                    + "'isisdist.astrogeology.usgs.gov' or 'isisdist.wr.usgs.gov'");
        }
    }

    /**
     * Sets the logger.
     *
     * @param logger
     *             the logger to set
     */
    public void setLogger(Log logger) {
        this.logger = logger;
    }

    /**
     * Sets the save directory.
     *
     * @param saveDirectory
     *             the save directory to set
     */
    public void setSaveDirectory(String saveDirectory) {
        this.saveDirectory = saveDirectory;
    }

    /**
     * Sets the server's name.
     *
     * @param serverName
     *             the server's name to set
     */
    public void setServerName(String serverName) {
        this.serverName = serverName;
    }
}

