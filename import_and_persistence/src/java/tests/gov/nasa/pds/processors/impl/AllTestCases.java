/*
 * Copyright (C) 2014 TopCoder Inc., All Rights Reserved.
 */
package gov.nasa.pds.processors.impl;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * <p>
 * Aggregates all tests in this package.
 * </p>
 *
 * @author caoweiquan322
 * @version 1.0
 */
public class AllTestCases extends TestCase {
    /**
     * Aggregates all tests in this class.
     *
     * @return Test suite aggregating all tests.
     */
    public static Test suite() {
        final TestSuite suite = new TestSuite();
        suite.addTest(LROCImportProcessorImplTest.suite());

        // We suggest run kernel command line tool instead to manually test this class
        // because it takes too much time (may be several hours). If your network condition
        // is not good, it may even take 1 day or longer.
        //suite.addTest(KernelImportProcessorImplTest.suite());
        return suite;
    }
}
