
package gov.nasa.pds;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for target complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="target">
 *   &lt;complexContent>
 *     &lt;extension base="{http://pds.nasa.gov/}namedEntity">
 *       &lt;sequence>
 *         &lt;element name="references" type="{http://pds.nasa.gov/}reference" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="types" type="{http://pds.nasa.gov/}targetType" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "target", propOrder = {
    "references",
    "types"
})
public class Target
    extends NamedEntity
{

    @XmlElement(nillable = true)
    protected List<Reference> references;
    @XmlElement(nillable = true)
    protected List<TargetType> types;

    /**
     * Gets the value of the references property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the references property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getReferences().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Reference }
     * 
     * 
     */
    public List<Reference> getReferences() {
        if (references == null) {
            references = new ArrayList<Reference>();
        }
        return this.references;
    }

    /**
     * Gets the value of the types property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the types property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getTypes().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link TargetType }
     * 
     * 
     */
    public List<TargetType> getTypes() {
        if (types == null) {
            types = new ArrayList<TargetType>();
        }
        return this.types;
    }

}
