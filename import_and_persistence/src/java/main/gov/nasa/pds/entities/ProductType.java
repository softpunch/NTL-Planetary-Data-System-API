/*
 * Copyright (C) 2014 TopCoder Inc., All Rights Reserved.
 */
package gov.nasa.pds.entities;

/**
 * This enumeration holds the product type of a {@link MapImage}. Refer to the <a
 * href="http://ode.rsl.wustl.edu/moon/indexProductSearch.aspx">Lunar Orbital Data Explorer product
 * search</a> for the full universe of Product Types.
 *
 * Thread Safety: This class is immutable and thread safe.
 *
 * @author schmoel, TCSDEVELOPER
 * @version 1.0
 */
public enum ProductType {
    /** Calibrated Data Record Narrow Angle Camera */
    CDRNAC,

    /** Calibrated Data Record Wide Angle Camera - Color */
    CDRWAC,

    /** Raw Data Record Narrow Angle Camera */
    EDRNAC,

    /** Raw Data Record Wide Angle Camera - Color */
    EDRWAC
}
