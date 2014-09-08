/*
 * Copyright (C) 2014 TopCoder Inc., All Rights Reserved.
 */
package gov.nasa.pds;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * <p>
 * Aggregates all tests of this assembly.
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
        suite.addTest(gov.nasa.pds.entities.AllTestCases.suite());
        suite.addTest(gov.nasa.pds.processors.impl.AllTestCases.suite());
        suite.addTest(gov.nasa.pds.services.impl.AllTestCases.suite());
        return suite;
    }
}
