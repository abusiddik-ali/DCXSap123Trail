//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.11 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2022.12.14 at 08:42:53 PM IST 
//


package de.hybris.platform.processengine.definition.xml;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for endState.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <p>
 * <pre>
 * &lt;simpleType name="endState"&gt;
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *     &lt;enumeration value="SUCCEEDED"/&gt;
 *     &lt;enumeration value="FAILED"/&gt;
 *     &lt;enumeration value="ERROR"/&gt;
 *   &lt;/restriction&gt;
 * &lt;/simpleType&gt;
 * </pre>
 * 
 */
@XmlType(name = "endState")
@XmlEnum
public enum EndState {

    SUCCEEDED,
    FAILED,
    ERROR;

    public String value() {
        return name();
    }

    public static EndState fromValue(String v) {
        return valueOf(v);
    }

}
