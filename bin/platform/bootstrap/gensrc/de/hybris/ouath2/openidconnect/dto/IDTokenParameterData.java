/*
 * ----------------------------------------------------------------
 * --- WARNING: THIS FILE IS GENERATED AND WILL BE OVERWRITTEN!
 * --- Generated at 21-Dec-2022, 9:05:21 PM
 * ----------------------------------------------------------------
 *
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.ouath2.openidconnect.dto;

import java.io.Serializable;


import java.util.Objects;
public  class IDTokenParameterData  implements Serializable 

{

	/** Default serialVersionUID value. */
 
	private static final long serialVersionUID = 1L;

	/** <i>Generated property</i> for <code>IDTokenParameterData.kid</code> property defined at extension <code>oauth2</code>. */
	
	private String kid;

	/** <i>Generated property</i> for <code>IDTokenParameterData.securityKeystoreLocation</code> property defined at extension <code>oauth2</code>. */
	
	private String securityKeystoreLocation;

	/** <i>Generated property</i> for <code>IDTokenParameterData.securityKeystorePassword</code> property defined at extension <code>oauth2</code>. */
	
	private String securityKeystorePassword;

	/** <i>Generated property</i> for <code>IDTokenParameterData.algorithm</code> property defined at extension <code>oauth2</code>. */
	
	private String algorithm;

	/** <i>Generated property</i> for <code>IDTokenParameterData.idTokenValiditySeconds</code> property defined at extension <code>oauth2</code>. */
	
	private int idTokenValiditySeconds;
	
	public IDTokenParameterData()
	{
		// default constructor
	}
	
	public void setKid(final String kid)
	{
		this.kid = kid;
	}

	public String getKid() 
	{
		return kid;
	}
	
	public void setSecurityKeystoreLocation(final String securityKeystoreLocation)
	{
		this.securityKeystoreLocation = securityKeystoreLocation;
	}

	public String getSecurityKeystoreLocation() 
	{
		return securityKeystoreLocation;
	}
	
	public void setSecurityKeystorePassword(final String securityKeystorePassword)
	{
		this.securityKeystorePassword = securityKeystorePassword;
	}

	public String getSecurityKeystorePassword() 
	{
		return securityKeystorePassword;
	}
	
	public void setAlgorithm(final String algorithm)
	{
		this.algorithm = algorithm;
	}

	public String getAlgorithm() 
	{
		return algorithm;
	}
	
	public void setIdTokenValiditySeconds(final int idTokenValiditySeconds)
	{
		this.idTokenValiditySeconds = idTokenValiditySeconds;
	}

	public int getIdTokenValiditySeconds() 
	{
		return idTokenValiditySeconds;
	}
	

}