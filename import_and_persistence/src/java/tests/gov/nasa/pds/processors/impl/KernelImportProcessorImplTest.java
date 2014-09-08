/*
 * Copyright (C) 2014 TopCoder Inc., All Rights Reserved.
 */
package gov.nasa.pds.processors.impl;

import gov.nasa.pds.processors.KernelImportProcessor;
import gov.nasa.pds.services.DataSetProcessingConfigurationException;

import junit.framework.JUnit4TestAdapter;

import org.junit.Before;
import org.junit.Test;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import com.topcoder.util.log.Log;

/**
 * <p>
 * The unit test of {@link KernelImportProcessorImpl} class.
 * </p>
 *
 * @author caoweiquan322
 * @version 1.0
 */
public class KernelImportProcessorImplTest {
    /**
     * The application context for test.
     */
    private ApplicationContext context;

    /**
     * The logger for test.
     */
    private Log logger;

    /**
     * Adapter for JUnit.
     *
     * @return a test suite.
     */
    public static junit.framework.Test suite() {
        return new JUnit4TestAdapter(KernelImportProcessorImplTest.class);
    }

    /**
     * Sets up the unit tests.
     *
     * @throws Exception to JUnit.
     */
    @Before
    public void setUp() throws Exception {
        context = new ClassPathXmlApplicationContext("applicationContext_import.xml");
        logger = (Log) context.getBean("logger");
    }

    /**
     * Test method <code>importMapData()</code>.
     *
     * @throws Exception to JUnit.
     */
    @Test
    public void testImportKernels() throws Exception {
        // Prepare the processor.
        KernelImportProcessor processor =
                (KernelImportProcessor) context.getBean("KernelImportProcessor");
        // Import kernels from server.
        processor.importKernels();
    }

    /**
     * Test method <code>afterPropertiesSet()</code> could check invalid properties.
     *
     * @throws Exception to JUnit.
     */
    @Test(expected = DataSetProcessingConfigurationException.class)
    public void testAfterPropertiesSet1() throws Exception {
        KernelImportProcessorImpl processor = new KernelImportProcessorImpl();
        processor.setLogger(null);
        processor.setSaveDirectory("~/kernels_data");
        processor.setServerName("isisdist.astrogeology.usgs.gov");
        processor.afterPropertiesSet();
    }

    /**
     * Test method <code>afterPropertiesSet()</code> could check invalid properties.
     *
     * @throws Exception to JUnit.
     */
    @Test(expected = DataSetProcessingConfigurationException.class)
    public void testAfterPropertiesSet2() throws Exception {
        KernelImportProcessorImpl processor = new KernelImportProcessorImpl();
        processor.setLogger(null);
        processor.setSaveDirectory("");
        processor.setServerName("isisdist.astrogeology.usgs.gov");
        processor.afterPropertiesSet();
    }

    /**
     * Test method <code>afterPropertiesSet()</code> could check invalid properties.
     *
     * @throws Exception to JUnit.
     */
    @Test(expected = DataSetProcessingConfigurationException.class)
    public void testAfterPropertiesSet3() throws Exception {
        KernelImportProcessorImpl processor = new KernelImportProcessorImpl();
        processor.setLogger(logger);
        processor.setSaveDirectory("~/kernels_data");
        processor.setServerName("");
        processor.afterPropertiesSet();
    }
}
