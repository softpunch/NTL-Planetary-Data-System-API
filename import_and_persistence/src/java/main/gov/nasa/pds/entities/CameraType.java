/*
 * Copyright (C) 2014 TopCoder Inc., All Rights Reserved.
 */
package gov.nasa.pds.entities;

/**
 * This new enumeration holds type of LRO camera(left or right).
 *
 * Thread Safety: This class is immutable and thread safe.
 *
 * @author fivestarwy, caoweiquan322
 * @version 1.0
 */
public enum CameraType {
    /**
     * The left camera.
     */
    LROC_LEFT,

    /**
     * The right camera.
     */
    LROC_RIGHT,

    /**
     * The unknown camera type.
     */
    LROC_UNKNOWN;
}
