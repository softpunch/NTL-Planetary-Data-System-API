/*
 * Copyright (C) 2014 TopCoder Inc., All Rights Reserved.
 */
package gov.nasa.pds.processors.impl;

import static org.junit.Assert.assertEquals;
import gov.nasa.pds.processors.LROCImportProcessor;
import gov.nasa.pds.services.ConversionPersistence;
import gov.nasa.pds.services.DataSetProcessingConfigurationException;
import gov.nasa.pds.services.impl.TestHelper;

import java.io.File;
import java.sql.Connection;
import java.sql.ResultSet;

import junit.framework.JUnit4TestAdapter;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import com.topcoder.util.log.Log;

/**
 * <p>
 * The unit test of {@link LROCImportProcessorImpl} class.
 * </p>
 *
 * @author caoweiquan322
 * @version 1.0
 */
public class LROCImportProcessorImplTest {
    /**
     * The sql connection for test.
     */
    private Connection conn;

    /**
     * The application context for test.
     */
    private ApplicationContext context;

    /**
     * The logger for test.
     */
    private Log logger;

    /**
     * The conversion persistence for test.
     */
    private ConversionPersistence conversionPersistence;

    /**
     * Adapter for JUnit.
     *
     * @return a test suite.
     */
    public static junit.framework.Test suite() {
        return new JUnit4TestAdapter(LROCImportProcessorImplTest.class);
    }

    /**
     * Sets up the unit tests.
     *
     * @throws Exception to JUnit.
     */
    @Before
    public void setUp() throws Exception {
        conn = TestHelper.getConnection();
        context = new ClassPathXmlApplicationContext("applicationContext_import.xml");
        logger = (Log) context.getBean("logger");
        conversionPersistence = (ConversionPersistence) context.getBean("conversionPersistence");

        TestHelper.execute(conn, "DELETE FROM `table_counter`");
        TestHelper.execute(conn, "INSERT INTO `table_counter` VALUES (1,0)");
    }

    /**
     * Tears down the test environment.
     *
     * @throws Exception to JUnit.
     */
    @After
    public void tearDown() throws Exception {
        try {
            TestHelper.deleteData(conn);
            TestHelper.execute(conn, "drop table IF EXISTS dummyTable");
        } finally {
            TestHelper.closeConnection(conn);
        }
    }

    /**
     * Test method <code>importMapData()</code>.
     *
     * @throws Exception to JUnit.
     */
    @Test
    public void testImportMapData() throws Exception {
        // Prepare the processor.
        LROCImportProcessor processor =
                (LROCImportProcessor) context.getBean("LROCImportProcessor");
        // Import data and check the database.
        processor.importMapData("CDRNAC", "NAC");
        // Check that three map_images were stored.
        ResultSet resultSet = TestHelper.query(conn, "Select * from map_image");
        int count = 0;
        while (resultSet.next()) {
            ++count;
        }
        assertEquals("Expect 3 images were parsed.", 3, count);
        // Remove the resulted tif files.
        removeFiles(".", ".tif");
    }

    /**
     * Test method <code>afterPropertiesSet()</code> could check invalid properties.
     *
     * @throws Exception to JUnit.
     */
    @Test(expected = DataSetProcessingConfigurationException.class)
    public void testAfterPropertiesSet1() throws Exception {
        LROCImportProcessorImpl processor = new LROCImportProcessorImpl();
        processor.setLogger(null);
        processor.setConversionPersistence(conversionPersistence);
        processor.setRequestUrl("http://www.google.com");
        processor.afterPropertiesSet();
    }

    /**
     * Test method <code>afterPropertiesSet()</code> could check invalid properties.
     *
     * @throws Exception to JUnit.
     */
    @Test(expected = DataSetProcessingConfigurationException.class)
    public void testAfterPropertiesSet2() throws Exception {
        LROCImportProcessorImpl processor = new LROCImportProcessorImpl();
        processor.setLogger(logger);
        processor.setConversionPersistence(null);
        processor.setRequestUrl("http://www.google.com");
        processor.afterPropertiesSet();
    }

    /**
     * Test method <code>afterPropertiesSet()</code> could check invalid properties.
     *
     * @throws Exception to JUnit.
     */
    @Test(expected = DataSetProcessingConfigurationException.class)
    public void testAfterPropertiesSet3() throws Exception {
        LROCImportProcessorImpl processor = new LROCImportProcessorImpl();
        processor.setLogger(logger);
        processor.setConversionPersistence(conversionPersistence);
        processor.setRequestUrl("Invalid URL");
        processor.afterPropertiesSet();
    }

    /**
     * Helper method used to delete specified files.
     *
     * @param strDir
     *             under which folder to delete files
     * @param suffix
     *             suffix of the files to delete
     * @throws Exception to JUnit.
     */
    private void removeFiles(String strDir, String suffix) throws Exception {
        File dir = new File(strDir);
        File[] files = dir.listFiles();
        for (File file : files) {
            if (file.isFile() && file.getName().endsWith(suffix)) {
                file.delete();
            }
        }
    }
}
