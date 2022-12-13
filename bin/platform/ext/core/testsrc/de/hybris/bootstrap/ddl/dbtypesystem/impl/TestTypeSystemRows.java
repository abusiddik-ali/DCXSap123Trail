/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.bootstrap.ddl.dbtypesystem.impl;

import java.sql.Timestamp;
import java.util.Collections;
import java.util.LinkedList;


public class TestTypeSystemRows implements RowsProvider
{
	private final LinkedList<DeploymentRow> deployments = new LinkedList<>();
	private final LinkedList<TypeRow> types = new LinkedList<>();
	private final LinkedList<AttributeRow> attributes = new LinkedList<>();

	public TestTypeSystemRows()
	{
		addDeployments();
		addTypes();
		addAttributes();
	}

	@Override
	public Iterable<DeploymentRow> getDeploymentRows()
	{
		return deployments;
	}

	@Override
	public Iterable<TypeRow> getTypeRows()
	{
		return types;
	}

	@Override
	public Iterable<AttributeRow> getAttributeRows()
	{
		return attributes;
	}


	@Override
	public Iterable<AtomicTypeRow> getAtomicTypeRows()
	{
		return Collections.EMPTY_LIST;
	}

	@Override
	public Iterable<CollectionTypeRow> getCollectionTypeRow()
	{
		return Collections.EMPTY_LIST;
	}

	@Override
	public Iterable<MapTypeRow> getMapTypeRows()
	{
		return Collections.EMPTY_LIST;
	}

	@Override
	public Iterable<EnumerationValueRow> getEnumerationValueRows()
	{
		return Collections.EMPTY_LIST;
	}

	@Override
	public Iterable<NumberSeriesRow> getNumberSeriesRows()
	{
		return Collections.EMPTY_LIST;
	}

	@Override
	public Iterable<PropsRow> getPropsRows()
	{
		return Collections.EMPTY_LIST;
	}

	private void addDeployments()
	{
		deployments.add(new DeploymentRow()
		{
			{
				setSupername("de.hybris.platform.persistence.ExtensibleItem");
				setPropstablename("props");
				setTypecode(new Integer(0));
				setModifiers(new Integer(1));
				setPackagename("de.hybris.platform.persistence.c2l");
				setName("LocalizableItem");
				setExtensionname("core");
			}
		});
		deployments.add(new DeploymentRow()
		{
			{
				setSupername("de.hybris.platform.persistence.GenericItem");
				setPropstablename("productprops");
				setTypecode(new Integer(1));
				setModifiers(new Integer(2));
				setTablename("products");
				setPackagename("de.hybris.platform.persistence");
				setName("core_Product");
				setExtensionname("core");
			}
		});
		deployments.add(new DeploymentRow()
		{
			{
				setPropstablename("props");
				setTypecode(new Integer(0));
				setModifiers(new Integer(1));
				setPackagename("de.hybris.platform.persistence");
				setName("Item");
				setExtensionname("core");
			}
		});
		deployments.add(new DeploymentRow()
		{
			{
				setSupername("de.hybris.platform.persistence.c2l.LocalizableItem");
				setPropstablename("props");
				setTypecode(new Integer(0));
				setModifiers(new Integer(1));
				setPackagename("de.hybris.platform.persistence.type");
				setName("TypeManagerManaged");
				setExtensionname("core");
			}
		});
		deployments.add(new DeploymentRow()
		{
			{
				setSupername("de.hybris.platform.persistence.c2l.LocalizableItem");
				setPropstablename("props");
				setTypecode(new Integer(99));
				setModifiers(new Integer(0));
				setTablename("genericitems");
				setPackagename("de.hybris.platform.persistence");
				setName("GenericItem");
				setExtensionname("core");
			}
		});
		deployments.add(new DeploymentRow()
		{
			{
				setSupername("de.hybris.platform.persistence.type.TypeManagerManaged");
				setPropstablename("props");
				setTypecode(new Integer(0));
				setModifiers(new Integer(1));
				setPackagename("de.hybris.platform.persistence.type");
				setName("Type");
				setExtensionname("core");
			}
		});
		deployments.add(new DeploymentRow()
		{
			{
				setSupername("de.hybris.platform.persistence.Item");
				setPropstablename("props");
				setTypecode(new Integer(0));
				setModifiers(new Integer(1));
				setPackagename("de.hybris.platform.persistence");
				setName("ExtensibleItem");
				setExtensionname("core");
			}
		});
		deployments.add(new DeploymentRow()
		{
			{
				setSupername("de.hybris.platform.persistence.type.HierarchieType");
				setPropstablename("props");
				setTypecode(new Integer(82));
				setModifiers(new Integer(0));
				setTablename("composedtypes");
				setPackagename("de.hybris.platform.persistence.type");
				setName("ComposedType");
				setExtensionname("core");
			}
		});
	}

	private void addTypes()
	{
		types.add(new TypeRow()
		{
			{
				setHjmpts(new Long(5));
				setTypepkstring(getComposedTypeTypeCodePK());
				setPk(getProductTypeCodePK());
				setCreatedts(new Timestamp(1380205274354L));
				setModifiedts(new Timestamp(1380205305787L));
				setAclts(new Long(0));
				setPropts(new Long(0));
				setInternalcode("Product");
				setInternalcodelowercase("product");
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093153362,8796094922834,");
				setSupertypepk(getGenericItemTypeCodePK());
				setItemjndiname("de.hybris.platform.persistence.core_Product");
				setJaloclassname("de.hybris.platform.jalo.product.Product");
				setRemovable(isTypeRemovable());
				setItemtypecode(new Integer(1));
				setSingleton(isTypeSingleton());
				setPropertytablestatus(isPropertyTableStatus());
				setAutocreate(isTypeAutoCreated());
				setExtensionname("core");
				setCatalogversionattributequali("catalogVersion");
				setGenerate(Boolean.TRUE);
				setUniquekeyattributequalifier("code");
				setCatalogitemtype(Boolean.TRUE);
			}
		});
		types.add(new TypeRow()
		{
			{
				setHjmpts(new Long(4));
				setTypepkstring(getComposedTypeTypeCodePK());
				setPk(getGenericItemTypeCodePK());
				setCreatedts(new Timestamp(1380205274189L));
				setModifiedts(new Timestamp(1380205303880L));
				setAclts(new Long(0));
				setPropts(new Long(0));
				setInternalcode("GenericItem");
				setInternalcodelowercase("genericitem");
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093153362,");
				setSupertypepk(getLocalizedItemTypeCodePK());
				setItemjndiname("de.hybris.platform.persistence.GenericItem");
				setJaloclassname("de.hybris.platform.jalo.GenericItem");
				setRemovable(isTypeRemovable());
				setItemtypecode(new Integer(99));
				setSingleton(isTypeSingleton());
				setPropertytablestatus(isPropertyTableStatus());
				setAutocreate(isTypeAutoCreated());
				setExtensionname("core");
				setGenerate(Boolean.FALSE);
			}
		});
		types.add(new TypeRow()
		{
			{
				setHjmpts(new Long(5));
				setTypepkstring(getComposedTypeTypeCodePK());
				setPk(getComposedTypeTypeCodePK());
				setCreatedts(new Timestamp(1380205274200L));
				setModifiedts(new Timestamp(1380205305995L));
				setAclts(new Long(0));
				setPropts(new Long(0));
				setInternalcode("ComposedType");
				setInternalcodelowercase("composedtype");
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,8796093218898,8796093251666,");
				setSupertypepk(getTypeTypeCodePK());
				setItemjndiname("de.hybris.platform.persistence.type.ComposedType");
				setJaloclassname("de.hybris.platform.jalo.type.ComposedType");
				setRemovable(isTypeRemovable());
				setItemtypecode(new Integer(82));
				setSingleton(isTypeSingleton());
				setPropertytablestatus(isPropertyTableStatus());
				setAutocreate(isTypeAutoCreated());
				setExtensionname("core");
				setSystemtype(isSystemType());
				setGenerate(Boolean.FALSE);
			}
		});
		types.add(new TypeRow()
		{
			{
				setHjmpts(new Long(4));
				setTypepkstring(getComposedTypeTypeCodePK());
				setPk(getLocalizedItemTypeCodePK());
				setCreatedts(new Timestamp(1380205274185L));
				setModifiedts(new Timestamp(1380205303682L));
				setAclts(new Long(0));
				setPropts(new Long(0));
				setInternalcode("LocalizableItem");
				setInternalcodelowercase("localizableitem");
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,");
				setSupertypepk(getExtensibleItemTypeCodePK());
				setItemjndiname("de.hybris.platform.persistence.c2l.LocalizableItem");
				setJaloclassname("de.hybris.platform.jalo.c2l.LocalizableItem");
				setRemovable(isTypeRemovable());
				setItemtypecode(new Integer(0));
				setSingleton(isTypeSingleton());
				setPropertytablestatus(isPropertyTableStatus());
				setAutocreate(isTypeAutoCreated());
				setExtensionname("core");
				setGenerate(Boolean.FALSE);
			}
		});
		types.add(new TypeRow()
		{
			{
				setHjmpts(new Long(5));
				setTypepkstring(getComposedTypeTypeCodePK());
				setPk(getTypeTypeCodePK());
				setCreatedts(new Timestamp(1380205274196L));
				setModifiedts(new Timestamp(1380205303718L));
				setAclts(new Long(0));
				setPropts(new Long(0));
				setInternalcode("Type");
				setInternalcodelowercase("type");
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,8796093218898,");
				setSupertypepk(getTypeManagerManagedTypeCodePK());
				setItemjndiname("de.hybris.platform.persistence.type.Type");
				setJaloclassname("de.hybris.platform.jalo.type.Type");
				setRemovable(isTypeRemovable());
				setItemtypecode(new Integer(0));
				setSingleton(isTypeSingleton());
				setPropertytablestatus(isPropertyTableStatus());
				setAutocreate(isTypeAutoCreated());
				setExtensionname("core");
				setSystemtype(isSystemType());
				setGenerate(Boolean.FALSE);
			}
		});
		types.add(new TypeRow()
		{
			{
				setHjmpts(new Long(4));
				setTypepkstring(getComposedTypeTypeCodePK());
				setPk(getExtensibleItemTypeCodePK());
				setCreatedts(new Timestamp(1380205274181L));
				setModifiedts(new Timestamp(1380205303572L));
				setAclts(new Long(0));
				setPropts(new Long(0));
				setInternalcode("ExtensibleItem");
				setInternalcodelowercase("extensibleitem");
				setInheritancepathstring(",8796093055058,8796093087826,");
				setSupertypepk(getItemTypeCodePK());
				setItemjndiname("de.hybris.platform.persistence.ExtensibleItem");
				setJaloclassname("de.hybris.platform.jalo.ExtensibleItem");
				setRemovable(isTypeRemovable());
				setItemtypecode(new Integer(0));
				setSingleton(isTypeSingleton());
				setPropertytablestatus(isPropertyTableStatus());
				setAutocreate(isTypeAutoCreated());
				setExtensionname("core");
				setGenerate(Boolean.FALSE);
			}
		});
		types.add(new TypeRow()
		{
			{
				setHjmpts(new Long(4));
				setTypepkstring(getComposedTypeTypeCodePK());
				setPk(getTypeManagerManagedTypeCodePK());
				setCreatedts(new Timestamp(1380205274192L));
				setModifiedts(new Timestamp(1380205303684L));
				setAclts(new Long(0));
				setPropts(new Long(0));
				setInternalcode("TypeManagerManaged");
				setInternalcodelowercase("typemanagermanaged");
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,");
				setSupertypepk(getLocalizedItemTypeCodePK());
				setItemjndiname("de.hybris.platform.persistence.type.TypeManagerManaged");
				setJaloclassname("de.hybris.platform.jalo.type.TypeManagerManaged");
				setRemovable(isTypeRemovable());
				setItemtypecode(new Integer(0));
				setSingleton(isTypeSingleton());
				setPropertytablestatus(isPropertyTableStatus());
				setAutocreate(isTypeAutoCreated());
				setExtensionname("core");
				setSystemtype(isSystemType());
				setGenerate(Boolean.FALSE);
			}
		});
		types.add(new TypeRow()
		{
			{
				setHjmpts(new Long(7));
				setTypepkstring(getComposedTypeTypeCodePK());
				setPk(getItemTypeCodePK());
				setCreatedts(new Timestamp(1380205274170L));
				setModifiedts(new Timestamp(1380205301629L));
				setAclts(new Long(0));
				setPropts(new Long(0));
				setInternalcode("Item");
				setInternalcodelowercase("item");
				setInheritancepathstring(",8796093055058,");
				setItemjndiname("de.hybris.platform.persistence.Item");
				setJaloclassname("de.hybris.platform.jalo.Item");
				setRemovable(isTypeRemovable());
				setItemtypecode(new Integer(0));
				setSingleton(isTypeSingleton());
				setPropertytablestatus(isPropertyTableStatus());
				setAutocreate(isTypeAutoCreated());
				setExtensionname("core");
				setGenerate(Boolean.FALSE);
			}
		});
	}

	private Long getExtensibleItemTypeCodePK()
	{
		return new Long(65618);
	}

	private Long getTypeTypeCodePK()
	{
		return new Long(196690);
	}

	private Long getComposedTypeTypeCodePK()
	{
		return new Long(229458);
	}

	private Long getTypeManagerManagedTypeCodePK()
	{
		return new Long(163922);
	}

	private Long getItemTypeCodePK()
	{
		return new Long(32850);
	}

	private Long getLocalizedItemTypeCodePK()
	{
		return new Long(98386);
	}

	private Long getGenericItemTypeCodePK()
	{
		return new Long(131154);
	}

	private Long getProductTypeCodePK()
	{
		return new Long(1900626);
	}

	private Boolean isSystemType()
	{
		return Boolean.TRUE;
	}

	private Boolean isTypeAutoCreated()
	{
		return Boolean.TRUE;
	}

	private Boolean isPropertyTableStatus()
	{
		return Boolean.TRUE;
	}

	private Boolean isTypeSingleton()
	{
		return Boolean.FALSE;
	}

	private Boolean isTypeRemovable()
	{
		return Boolean.FALSE;
	}

	private void addAttributes()
	{
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(4));
				setTypepkstring(new Long(655442));
				setPk(new Long(310345815));
				setCreatedts(new Timestamp(1380205281393L));
				setModifiedts(new Timestamp(1380205305846L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("productreferences");
				setQualifierinternal("productReferences");
				setAttributetypepk(new Long(5439571));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getProductTypeCodePK());
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(47));
				setRelationname("ProductReferenceRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setOrdered(isOrdered());
				setRelationtype(new Long(9338962));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(297238615));
				setCreatedts(new Timestamp(1380205281199L));
				setModifiedts(new Timestamp(1380205305854L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("supplieralternativeaid");
				setQualifierinternal("supplierAlternativeAID");
				setAttributetypepk(new Long(327761));
				setIshidden(isAttributeShown());
				setColumnname("p_supplieralternativeaid");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(265060439));
				setCreatedts(new Timestamp(1380205280713L));
				setModifiedts(new Timestamp(1380205303478L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("synchronizedcopies");
				setQualifierinternal("synchronizedCopies");
				setAttributetypepk(new Long(2326611));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getProductTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093153362,8796094922834,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(256213079));
				setModifiers(new Integer(9229));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(298614871));
				setCreatedts(new Timestamp(1380205281218L));
				setModifiedts(new Timestamp(1380205305858L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("thumbnails");
				setQualifierinternal("thumbnails");
				setAttributetypepk(new Long(163923));
				setIshidden(isAttributeShown());
				setColumnname("p_thumbnails");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(524369));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(297107543));
				setCreatedts(new Timestamp(1380205281197L));
				setModifiedts(new Timestamp(1380205305837L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("offlinedate");
				setQualifierinternal("offlineDate");
				setAttributetypepk(new Long(393297));
				setIshidden(isAttributeShown());
				setColumnname("p_offlinedate");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(393297));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(296910935));
				setCreatedts(new Timestamp(1380205281195L));
				setModifiedts(new Timestamp(1380205291259L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("catalog");
				setQualifierinternal("catalog");
				setAttributetypepk(new Long(7635026));
				setIshidden(isAttributeHidden());
				setColumnname("p_catalog");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(407));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(297435223));
				setCreatedts(new Timestamp(1380205281202L));
				setModifiedts(new Timestamp(1380205305828L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("manufacturername");
				setQualifierinternal("manufacturerName");
				setAttributetypepk(new Long(327761));
				setIshidden(isAttributeShown());
				setColumnname("p_manufacturername");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(297173079));
				setCreatedts(new Timestamp(1380205281198L));
				setModifiedts(new Timestamp(1380205305809L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("ean");
				setQualifierinternal("ean");
				setAttributetypepk(new Long(327761));
				setIshidden(isAttributeShown());
				setColumnname("p_ean");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(43155543));
				setCreatedts(new Timestamp(1380205276912L));
				setModifiedts(new Timestamp(1380205302501L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("itemtype");
				setQualifierinternal("itemtype");
				setAttributetypepk(getComposedTypeTypeCodePK());
				setIshidden(isAttributeShown());
				setColumnname("TypePkString");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093153362,8796094922834,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("typePkString");
				setSuperattributedescriptorpk(new Long(34308183));
				setModifiers(new Integer(1055));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(188055639));
				setCreatedts(new Timestamp(1380205279613L));
				setModifiedts(new Timestamp(1380205301736L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("alldocuments");
				setQualifierinternal("allDocuments");
				setAttributetypepk(new Long(4227155));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getProductTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093153362,8796094922834,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(179208279));
				setModifiers(new Integer(1071));
				setRelationname("ItemDocrRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("commons");
				setOrdered(isNotOrdered());
				setRelationtype(new Long(4128850));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(299335767));
				setCreatedts(new Timestamp(1380205281227L));
				setModifiedts(new Timestamp(1380205305865L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("xmlcontent");
				setQualifierinternal("xmlcontent");
				setAttributetypepk(new Long(327761));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(8479));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(151912535));
				setCreatedts(new Timestamp(1380205278993L));
				setModifiedts(new Timestamp(1380205305833L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("name");
				setQualifierinternal("name");
				setAttributetypepk(new Long(65620));
				setIshidden(isAttributeShown());
				setColumnname("p_name");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(799));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(314802263));
				setCreatedts(new Timestamp(1380205281472L));
				setModifiedts(new Timestamp(1380205305814L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("europe1discounts");
				setQualifierinternal("europe1Discounts");
				setAttributetypepk(new Long(2818131));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getProductTypeCodePK());
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(8239));
				setAutocreate(isAutoCreate());
				setExtensionname("europe1");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(298942551));
				setCreatedts(new Timestamp(1380205281222L));
				setModifiedts(new Timestamp(1380205305819L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("features");
				setQualifierinternal("features");
				setAttributetypepk(new Long(2031699));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getProductTypeCodePK());
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(8239));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(297304151));
				setCreatedts(new Timestamp(1380205281200L));
				setModifiedts(new Timestamp(1380205305796L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("buyerids");
				setQualifierinternal("buyerIDS");
				setAttributetypepk(new Long(688212));
				setIshidden(isAttributeShown());
				setColumnname("p_buyerids");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(32849));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(151846999));
				setCreatedts(new Timestamp(1380205278991L));
				setModifiedts(new Timestamp(1380205305801L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("code");
				setQualifierinternal("code");
				setAttributetypepk(new Long(327761));
				setIshidden(isAttributeShown());
				setColumnname("Code");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(2327));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
				setUnique(isUnique());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(314736727));
				setCreatedts(new Timestamp(1380205281472L));
				setModifiedts(new Timestamp(1380205305817L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("europe1taxes");
				setQualifierinternal("europe1Taxes");
				setAttributetypepk(new Long(2785363));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getProductTypeCodePK());
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(8239));
				setAutocreate(isAutoCreate());
				setExtensionname("europe1");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(297762903));
				setCreatedts(new Timestamp(1380205281207L));
				setModifiedts(new Timestamp(1380205305850L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("specialtreatmentclasses");
				setQualifierinternal("specialTreatmentClasses");
				setAttributetypepk(new Long(720980));
				setIshidden(isAttributeShown());
				setColumnname("p_specialtreatmentclasses");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(32849));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(152043607));
				setCreatedts(new Timestamp(1380205278995L));
				setModifiedts(new Timestamp(1380205305806L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("description");
				setQualifierinternal("description");
				setAttributetypepk(new Long(65620));
				setIshidden(isAttributeShown());
				setColumnname("p_description");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(799));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(298811479));
				setCreatedts(new Timestamp(1380205281220L));
				setModifiedts(new Timestamp(1380205305804L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("data_sheet");
				setQualifierinternal("data_sheet");
				setAttributetypepk(new Long(163923));
				setIshidden(isAttributeShown());
				setColumnname("p_data_sheet");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(524369));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(376111191));
				setCreatedts(new Timestamp(1380205282308L));
				setModifiedts(new Timestamp(1380205305821L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("inittestaddeddumpattr");
				setQualifierinternal("initTestAddedDumpAttr");
				setAttributetypepk(new Long(327761));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(8479));
				setAutocreate(isAutoCreate());
				setExtensionname("persistencetest");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(298352727));
				setCreatedts(new Timestamp(1380205281214L));
				setModifiedts(new Timestamp(1380205305831L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("maxorderquantity");
				setQualifierinternal("maxOrderQuantity");
				setAttributetypepk(new Long(98385));
				setIshidden(isAttributeShown());
				setColumnname("p_maxorderquantity");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(98385));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(314474583));
				setCreatedts(new Timestamp(1380205281468L));
				setModifiedts(new Timestamp(1380205305791L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("europe1pricefactory_ppg");
				setQualifierinternal("Europe1PriceFactory_PPG");
				setAttributetypepk(new Long(10190930));
				setIshidden(isAttributeShown());
				setColumnname("p_europe1pricefactory_ppg");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("europe1");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(299008087));
				setCreatedts(new Timestamp(1380205281223L));
				setModifiedts(new Timestamp(1380205305860L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("untypedfeatures");
				setQualifierinternal("untypedFeatures");
				setAttributetypepk(new Long(2031699));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getProductTypeCodePK());
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(8239));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(60293207));
				setCreatedts(new Timestamp(1380205277273L));
				setModifiedts(new Timestamp(1380205302865L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("owner");
				setQualifierinternal("owner");
				setAttributetypepk(getItemTypeCodePK());
				setIshidden(isAttributeShown());
				setColumnname("OwnerPkString");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093153362,8796094922834,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("ownerPkString");
				setSuperattributedescriptorpk(new Long(51445847));
				setModifiers(new Integer(3101));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(314540119));
				setCreatedts(new Timestamp(1380205281469L));
				setModifiedts(new Timestamp(1380205305792L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("europe1pricefactory_ptg");
				setQualifierinternal("Europe1PriceFactory_PTG");
				setAttributetypepk(new Long(10125394));
				setIshidden(isAttributeShown());
				setColumnname("p_europe1pricefactory_ptg");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("europe1");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(297697367));
				setCreatedts(new Timestamp(1380205281206L));
				setModifiedts(new Timestamp(1380205305805L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("deliverytime");
				setQualifierinternal("deliveryTime");
				setAttributetypepk(new Long(196689));
				setIshidden(isAttributeShown());
				setColumnname("p_deliverytime");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(196689));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(297631831));
				setCreatedts(new Timestamp(1380205281205L));
				setModifiedts(new Timestamp(1380205305813L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("erpgroupsupplier");
				setQualifierinternal("erpGroupSupplier");
				setAttributetypepk(new Long(327761));
				setIshidden(isAttributeShown());
				setColumnname("p_erpgroupsupplier");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(297828439));
				setCreatedts(new Timestamp(1380205281207L));
				setModifiedts(new Timestamp(1380205305847L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("remarks");
				setQualifierinternal("remarks");
				setAttributetypepk(new Long(65620));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(8991));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(331415639));
				setCreatedts(new Timestamp(1380205281727L));
				setModifiedts(new Timestamp(1380205302128L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("comments");
				setQualifierinternal("comments");
				setAttributetypepk(new Long(6455379));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getProductTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093153362,8796094922834,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(322568279));
				setModifiers(new Integer(1055));
				setRelationname("CommentItemRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("comments");
				setOrdered(isOrdered());
				setRelationtype(new Long(11862098));
				setGenerate(isGenerate());
				setIssource(isNotSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(168362071));
				setCreatedts(new Timestamp(1380205279260L));
				setModifiedts(new Timestamp(1380205303180L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("savedvalues");
				setQualifierinternal("savedValues");
				setAttributetypepk(new Long(3833939));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getProductTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093153362,8796094922834,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(159514711));
				setModifiers(new Integer(1037));
				setRelationname("ItemSavedValuesRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setOrdered(isOrdered());
				setRelationtype(new Long(2818130));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(296976471));
				setCreatedts(new Timestamp(1380205281196L));
				setModifiedts(new Timestamp(1380205305797L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("catalogversion");
				setQualifierinternal("catalogVersion");
				setAttributetypepk(new Long(7766098));
				setIshidden(isAttributeShown());
				setColumnname("p_catalogversion");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(279));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
				setUnique(isUnique());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(298877015));
				setCreatedts(new Timestamp(1380205281221L));
				setModifiedts(new Timestamp(1380205305842L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("others");
				setQualifierinternal("others");
				setAttributetypepk(new Long(163923));
				setIshidden(isAttributeShown());
				setColumnname("p_others");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(524369));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(298680407));
				setCreatedts(new Timestamp(1380205281219L));
				setModifiedts(new Timestamp(1380205305808L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("detail");
				setQualifierinternal("detail");
				setAttributetypepk(new Long(163923));
				setIshidden(isAttributeShown());
				setColumnname("p_detail");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(524369));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(77430871));
				setCreatedts(new Timestamp(1380205277628L));
				setModifiedts(new Timestamp(1380205303022L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("pk");
				setQualifierinternal("pk");
				setAttributetypepk(new Long(622673));
				setIshidden(isAttributeShown());
				setColumnname("PK");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(622673));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093153362,8796094922834,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("pkString");
				setSuperattributedescriptorpk(new Long(68583511));
				setModifiers(new Integer(1045));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(297500759));
				setCreatedts(new Timestamp(1380205281203L));
				setModifiedts(new Timestamp(1380205305830L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("manufacturertypedescription");
				setQualifierinternal("manufacturerTypeDescription");
				setAttributetypepk(new Long(65620));
				setIshidden(isAttributeShown());
				setColumnname("p_manufacturertypedescription");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(799));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(297893975));
				setCreatedts(new Timestamp(1380205281208L));
				setModifiedts(new Timestamp(1380205305849L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("segment");
				setQualifierinternal("segment");
				setAttributetypepk(new Long(65620));
				setIshidden(isAttributeShown());
				setColumnname("p_segment");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(799));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(299139159));
				setCreatedts(new Timestamp(1380205281224L));
				setModifiedts(new Timestamp(1380205305799L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("classificationclasses");
				setQualifierinternal("classificationClasses");
				setAttributetypepk(new Long(2490451));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getProductTypeCodePK());
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(8205));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(298483799));
				setCreatedts(new Timestamp(1380205281216L));
				setModifiedts(new Timestamp(1380205305845L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("pricequantity");
				setQualifierinternal("priceQuantity");
				setAttributetypepk(new Long(196689));
				setIshidden(isAttributeShown());
				setColumnname("p_pricequantity");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(196689));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
				setDefaultvaluedefinitionstring("new Double( 1 )");
				setDefaultvalue(new byte[]
						{ -84, -19, 0, 5, 115, 114, 0, 16, 106, 97, 118, 97, 46, 108, 97, 110, 103, 46, 68, 111, 117, 98, 108, 101,
								-128,
								-77, -62, 74, 41, 107, -5, 4, 2, 0, 1, 68, 0, 5, 118, 97, 108, 117, 101, 120, 114, 0, 16, 106, 97,
								118, 97, 46,
								108, 97, 110, 103, 46, 78, 117, 109, 98, 101, 114, -122, -84, -107, 29, 11, -108, -32, -117, 2, 0, 0,
								120, 112,
								63, -16, 0, 0, 0, 0, 0, 0 });
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(298025047));
				setCreatedts(new Timestamp(1380205281210L));
				setModifiedts(new Timestamp(1380205305795L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("articlestatus");
				setQualifierinternal("articleStatus");
				setAttributetypepk(new Long(655444));
				setIshidden(isAttributeShown());
				setColumnname("p_articlestatus");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(32849));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(799));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(303431767));
				setCreatedts(new Timestamp(1380205281279L));
				setModifiedts(new Timestamp(1380205305861L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("varianttype");
				setQualifierinternal("variantType");
				setAttributetypepk(new Long(8224850));
				setIshidden(isAttributeShown());
				setColumnname("p_varianttype");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(297369687));
				setCreatedts(new Timestamp(1380205281201L));
				setModifiedts(new Timestamp(1380205305827L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("manufactureraid");
				setQualifierinternal("manufacturerAID");
				setAttributetypepk(new Long(327761));
				setIshidden(isAttributeShown());
				setColumnname("p_manufactureraid");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(152174679));
				setCreatedts(new Timestamp(1380205278998L));
				setModifiedts(new Timestamp(1380205305844L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("picture");
				setQualifierinternal("picture");
				setAttributetypepk(new Long(1867858));
				setIshidden(isAttributeShown());
				setColumnname("p_picture");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(4));
				setTypepkstring(new Long(655442));
				setPk(new Long(308478039));
				setCreatedts(new Timestamp(1380205281357L));
				setModifiedts(new Timestamp(1380205305824L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("keywords");
				setQualifierinternal("keywords");
				setAttributetypepk(new Long(1048660));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getProductTypeCodePK());
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(543));
				setRelationname("Product2KeywordRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setOrdered(isOrdered());
				setRelationtype(new Long(9076818));
				setGenerate(isGenerate());
				setIssource(isNotSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(297042007));
				setCreatedts(new Timestamp(1380205281196L));
				setModifiedts(new Timestamp(1380205305839L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("onlinedate");
				setQualifierinternal("onlineDate");
				setAttributetypepk(new Long(393297));
				setIshidden(isAttributeShown());
				setColumnname("p_onlinedate");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(393297));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(152109143));
				setCreatedts(new Timestamp(1380205278997L));
				setModifiedts(new Timestamp(1380205305856L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("thumbnail");
				setQualifierinternal("thumbnail");
				setAttributetypepk(new Long(1867858));
				setIshidden(isAttributeShown());
				setColumnname("p_thumbnail");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(4));
				setTypepkstring(new Long(655442));
				setPk(new Long(383909975));
				setCreatedts(new Timestamp(1380205282416L));
				setModifiedts(new Timestamp(1380205305823L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("itrelationmanyend");
				setQualifierinternal("itRelationManyEnd");
				setAttributetypepk(new Long(8749139));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getProductTypeCodePK());
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(15));
				setRelationname("ITAllTypesOneToMany");
				setAutocreate(isAutoCreate());
				setExtensionname("persistencetest");
				setOrdered(isNotOrdered());
				setRelationtype(new Long(16973906));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(298090583));
				setCreatedts(new Timestamp(1380205281211L));
				setModifiedts(new Timestamp(1380205305794L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("approvalstatus");
				setQualifierinternal("approvalStatus");
				setAttributetypepk(new Long(9699410));
				setIshidden(isAttributeShown());
				setColumnname("p_approvalstatus");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(279));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
				setDefaultvaluedefinitionstring("em().getEnumerationValue(\"ArticleApprovalStatus\",\"check\")");
				setDefaultvalue(new byte[]
						{ -84, -19, 0, 5, 115, 114, 0, 41, 100, 101, 46, 104, 121, 98, 114, 105, 115, 46, 112, 108, 97, 116, 102, 111,
								114,
								109, 46, 117, 116, 105, 108, 46, 73, 116, 101, 109, 80, 114, 111, 112, 101, 114, 116, 121, 86, 97,
								108, 117,
								101, -44, -37, -75, 59, -44, -17, 52, -124, 2, 0, 1, 76, 0, 2, 112, 107, 116, 0, 28, 76, 100, 101, 47,
								104,
								121, 98, 114, 105, 115, 47, 112, 108, 97, 116, 102, 111, 114, 109, 47, 99, 111, 114, 101, 47, 80, 75,
								59, 120,
								112, 115, 114, 0, 26, 100, 101, 46, 104, 121, 98, 114, 105, 115, 46, 112, 108, 97, 116, 102, 111, 114,
								109, 46,
								99, 111, 114, 101, 46, 80, 75, -62, -38, 114, 44, -24, -103, 44, -110, 2, 0, 1, 74, 0, 9, 108, 111,
								110, 103,
								86, 97, 108, 117, 101, 120, 112, 0, 0, 8, 0, 0, 60, -128, 91 });
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(314671191));
				setCreatedts(new Timestamp(1380205281471L));
				setModifiedts(new Timestamp(1380205305816L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("europe1prices");
				setQualifierinternal("europe1Prices");
				setAttributetypepk(new Long(2752595));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getProductTypeCodePK());
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(8239));
				setAutocreate(isAutoCreate());
				setExtensionname("europe1");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(151978071));
				setCreatedts(new Timestamp(1380205278994L));
				setModifiedts(new Timestamp(1380205305859L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("unit");
				setQualifierinternal("unit");
				setAttributetypepk(new Long(1933394));
				setIshidden(isAttributeShown());
				setColumnname("UnitPK");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(299073623));
				setCreatedts(new Timestamp(1380205281224L));
				setModifiedts(new Timestamp(1380205305800L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("classificationindexstring");
				setQualifierinternal("classificationIndexString");
				setAttributetypepk(new Long(65620));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getProductTypeCodePK());
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(8717));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(314605655));
				setCreatedts(new Timestamp(1380205281470L));
				setModifiedts(new Timestamp(1380205305789L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("europe1pricefactory_pdg");
				setQualifierinternal("Europe1PriceFactory_PDG");
				setAttributetypepk(new Long(10256466));
				setIshidden(isAttributeShown());
				setColumnname("p_europe1pricefactory_pdg");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("europe1");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(26017879));
				setCreatedts(new Timestamp(1380205276461L));
				setModifiedts(new Timestamp(1380205302673L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("modifiedtime");
				setQualifierinternal("modifiedtime");
				setAttributetypepk(new Long(393297));
				setIshidden(isAttributeShown());
				setColumnname("modifiedTS");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(393297));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093153362,8796094922834,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("modifiedTimestampInternal");
				setSuperattributedescriptorpk(new Long(17170519));
				setModifiers(new Integer(1045));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(297566295));
				setCreatedts(new Timestamp(1380205281204L));
				setModifiedts(new Timestamp(1380205305812L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("erpgroupbuyer");
				setQualifierinternal("erpGroupBuyer");
				setAttributetypepk(new Long(327761));
				setIshidden(isAttributeShown());
				setColumnname("p_erpgroupbuyer");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(298156119));
				setCreatedts(new Timestamp(1380205281212L));
				setModifiedts(new Timestamp(1380205305803L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("contentunit");
				setQualifierinternal("contentUnit");
				setAttributetypepk(new Long(1933394));
				setIshidden(isAttributeShown());
				setColumnname("p_contentunit");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(383680599));
				setCreatedts(new Timestamp(1380205282411L));
				setModifiedts(new Timestamp(1380205305855L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("targetproducts");
				setQualifierinternal("targetProducts");
				setAttributetypepk(new Long(8618067));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getProductTypeCodePK());
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(31));
				setRelationname("TestManyToManyRelationOrderedSource");
				setAutocreate(isAutoCreate());
				setExtensionname("persistencetest");
				setOrdered(isNotOrdered());
				setRelationtype(new Long(16908370));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(298418263));
				setCreatedts(new Timestamp(1380205281215L));
				setModifiedts(new Timestamp(1380205305841L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("orderquantityinterval");
				setQualifierinternal("orderQuantityInterval");
				setAttributetypepk(new Long(98385));
				setIshidden(isAttributeShown());
				setColumnname("p_orderquantityinterval");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(98385));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(298745943));
				setCreatedts(new Timestamp(1380205281219L));
				setModifiedts(new Timestamp(1380205305826L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("logo");
				setQualifierinternal("logo");
				setAttributetypepk(new Long(163923));
				setIshidden(isAttributeShown());
				setColumnname("p_logo");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(524369));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(282198103));
				setCreatedts(new Timestamp(1380205280966L));
				setModifiedts(new Timestamp(1380205303333L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("synchronizationsources");
				setQualifierinternal("synchronizationSources");
				setAttributetypepk(new Long(2326611));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getProductTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093153362,8796094922834,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(273350743));
				setModifiers(new Integer(9229));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(299270231));
				setCreatedts(new Timestamp(1380205281226L));
				setModifiedts(new Timestamp(1380205305810L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("endlinenumber");
				setQualifierinternal("endLineNumber");
				setAttributetypepk(new Long(98385));
				setIshidden(isAttributeShown());
				setColumnname("p_endlinenumber");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(98385));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(299204695));
				setCreatedts(new Timestamp(1380205281225L));
				setModifiedts(new Timestamp(1380205305851L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("startlinenumber");
				setQualifierinternal("startLineNumber");
				setAttributetypepk(new Long(98385));
				setIshidden(isAttributeShown());
				setColumnname("p_startlinenumber");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(98385));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(4));
				setTypepkstring(new Long(655442));
				setPk(new Long(310706263));
				setCreatedts(new Timestamp(1380205281401L));
				setModifiedts(new Timestamp(1380205305853L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("supercategories");
				setQualifierinternal("supercategories");
				setAttributetypepk(new Long(5603411));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getProductTypeCodePK());
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(31));
				setRelationname("CategoryProductRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setOrdered(isNotOrdered());
				setRelationtype(new Long(9437266));
				setGenerate(isGenerate());
				setIssource(isNotSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(297959511));
				setCreatedts(new Timestamp(1380205281209L));
				setModifiedts(new Timestamp(1380205305840L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("order");
				setQualifierinternal("order");
				setAttributetypepk(new Long(98385));
				setIshidden(isAttributeShown());
				setColumnname("p_order");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(98385));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(376045655));
				setCreatedts(new Timestamp(1380205282307L));
				setModifiedts(new Timestamp(1380205305820L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("inittestaddedattr");
				setQualifierinternal("initTestAddedAttr");
				setAttributetypepk(new Long(327761));
				setIshidden(isAttributeShown());
				setColumnname("p_inittestaddedattr");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("persistencetest");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(298287191));
				setCreatedts(new Timestamp(1380205281214L));
				setModifiedts(new Timestamp(1380205305832L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("minorderquantity");
				setQualifierinternal("minOrderQuantity");
				setAttributetypepk(new Long(98385));
				setIshidden(isAttributeShown());
				setColumnname("p_minorderquantity");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(98385));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(303366231));
				setCreatedts(new Timestamp(1380205281277L));
				setModifiedts(new Timestamp(1380205305863L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("variants");
				setQualifierinternal("variants");
				setAttributetypepk(new Long(2687059));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getProductTypeCodePK());
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(8239));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(356221015));
				setCreatedts(new Timestamp(1380205282058L));
				setModifiedts(new Timestamp(1380205301935L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("assignedcockpititemtemplates");
				setQualifierinternal("assignedCockpitItemTemplates");
				setAttributetypepk(new Long(7340115));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getProductTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093153362,8796094922834,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(347373655));
				setModifiers(new Integer(1055));
				setRelationname("Item2CockpitItemTemplateRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("cockpit");
				setOrdered(isOrdered());
				setRelationtype(new Long(13795410));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(8880215));
				setCreatedts(new Timestamp(1380205275902L));
				setModifiedts(new Timestamp(1380205302313L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("creationtime");
				setQualifierinternal("creationtime");
				setAttributetypepk(new Long(393297));
				setIshidden(isAttributeShown());
				setColumnname("createdTS");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(393297));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093153362,8796094922834,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("creationTimestampInternal");
				setSuperattributedescriptorpk(new Long(32855));
				setModifiers(new Integer(3101));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(298221655));
				setCreatedts(new Timestamp(1380205281213L));
				setModifiedts(new Timestamp(1380205305836L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("numbercontentunits");
				setQualifierinternal("numberContentUnits");
				setAttributetypepk(new Long(196689));
				setIshidden(isAttributeShown());
				setColumnname("p_numbercontentunits");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(196689));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(298549335));
				setCreatedts(new Timestamp(1380205281217L));
				setModifiedts(new Timestamp(1380205305835L));
				setOwnerpkstring(getProductTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("normal");
				setQualifierinternal("normal");
				setAttributetypepk(new Long(163923));
				setIshidden(isAttributeShown());
				setColumnname("p_normal");
				setEnclosingtypepk(getProductTypeCodePK());
				setPersistencetypepk(new Long(524369));
				setInheritancepathstring(",8796094922834,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(331251799));
				setCreatedts(new Timestamp(1380205281725L));
				setModifiedts(new Timestamp(1380205302127L));
				setOwnerpkstring(getGenericItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("comments");
				setQualifierinternal("comments");
				setAttributetypepk(new Long(6455379));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getGenericItemTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093153362,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(322568279));
				setModifiers(new Integer(1055));
				setRelationname("CommentItemRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("comments");
				setOrdered(isOrdered());
				setRelationtype(new Long(11862098));
				setGenerate(isGenerate());
				setIssource(isNotSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(8716375));
				setCreatedts(new Timestamp(1380205275897L));
				setModifiedts(new Timestamp(1380205302313L));
				setOwnerpkstring(getGenericItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("creationtime");
				setQualifierinternal("creationtime");
				setAttributetypepk(new Long(393297));
				setIshidden(isAttributeShown());
				setColumnname("createdTS");
				setEnclosingtypepk(getGenericItemTypeCodePK());
				setPersistencetypepk(new Long(393297));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093153362,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("creationTimestampInternal");
				setSuperattributedescriptorpk(new Long(32855));
				setModifiers(new Integer(3101));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(187891799));
				setCreatedts(new Timestamp(1380205279611L));
				setModifiedts(new Timestamp(1380205301735L));
				setOwnerpkstring(getGenericItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("alldocuments");
				setQualifierinternal("allDocuments");
				setAttributetypepk(new Long(4227155));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getGenericItemTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093153362,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(179208279));
				setModifiers(new Integer(1071));
				setRelationname("ItemDocrRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("commons");
				setOrdered(isNotOrdered());
				setRelationtype(new Long(4128850));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(168198231));
				setCreatedts(new Timestamp(1380205279258L));
				setModifiedts(new Timestamp(1380205303179L));
				setOwnerpkstring(getGenericItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("savedvalues");
				setQualifierinternal("savedValues");
				setAttributetypepk(new Long(3833939));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getGenericItemTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093153362,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(159514711));
				setModifiers(new Integer(1037));
				setRelationname("ItemSavedValuesRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setOrdered(isOrdered());
				setRelationtype(new Long(2818130));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(282034263));
				setCreatedts(new Timestamp(1380205280964L));
				setModifiedts(new Timestamp(1380205303332L));
				setOwnerpkstring(getGenericItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("synchronizationsources");
				setQualifierinternal("synchronizationSources");
				setAttributetypepk(new Long(2326611));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getGenericItemTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093153362,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(273350743));
				setModifiers(new Integer(9229));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(25854039));
				setCreatedts(new Timestamp(1380205276456L));
				setModifiedts(new Timestamp(1380205302673L));
				setOwnerpkstring(getGenericItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("modifiedtime");
				setQualifierinternal("modifiedtime");
				setAttributetypepk(new Long(393297));
				setIshidden(isAttributeShown());
				setColumnname("modifiedTS");
				setEnclosingtypepk(getGenericItemTypeCodePK());
				setPersistencetypepk(new Long(393297));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093153362,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("modifiedTimestampInternal");
				setSuperattributedescriptorpk(new Long(17170519));
				setModifiers(new Integer(1045));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(77267031));
				setCreatedts(new Timestamp(1380205277625L));
				setModifiedts(new Timestamp(1380205303022L));
				setOwnerpkstring(getGenericItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("pk");
				setQualifierinternal("pk");
				setAttributetypepk(new Long(622673));
				setIshidden(isAttributeShown());
				setColumnname("PK");
				setEnclosingtypepk(getGenericItemTypeCodePK());
				setPersistencetypepk(new Long(622673));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093153362,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("pkString");
				setSuperattributedescriptorpk(new Long(68583511));
				setModifiers(new Integer(1045));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(60129367));
				setCreatedts(new Timestamp(1380205277270L));
				setModifiedts(new Timestamp(1380205302864L));
				setOwnerpkstring(getGenericItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("owner");
				setQualifierinternal("owner");
				setAttributetypepk(getItemTypeCodePK());
				setIshidden(isAttributeShown());
				setColumnname("OwnerPkString");
				setEnclosingtypepk(getGenericItemTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093153362,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("ownerPkString");
				setSuperattributedescriptorpk(new Long(51445847));
				setModifiers(new Integer(3101));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(356057175));
				setCreatedts(new Timestamp(1380205282057L));
				setModifiedts(new Timestamp(1380205301935L));
				setOwnerpkstring(getGenericItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("assignedcockpititemtemplates");
				setQualifierinternal("assignedCockpitItemTemplates");
				setAttributetypepk(new Long(7340115));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getGenericItemTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093153362,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(347373655));
				setModifiers(new Integer(1055));
				setRelationname("Item2CockpitItemTemplateRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("cockpit");
				setOrdered(isOrdered());
				setRelationtype(new Long(13795410));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(264896599));
				setCreatedts(new Timestamp(1380205280711L));
				setModifiedts(new Timestamp(1380205303478L));
				setOwnerpkstring(getGenericItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("synchronizedcopies");
				setQualifierinternal("synchronizedCopies");
				setAttributetypepk(new Long(2326611));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getGenericItemTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093153362,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(256213079));
				setModifiers(new Integer(9229));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(42991703));
				setCreatedts(new Timestamp(1380205276893L));
				setModifiedts(new Timestamp(1380205302500L));
				setOwnerpkstring(getGenericItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("itemtype");
				setQualifierinternal("itemtype");
				setAttributetypepk(getComposedTypeTypeCodePK());
				setIshidden(isAttributeShown());
				setColumnname("TypePkString");
				setEnclosingtypepk(getGenericItemTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093153362,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("typePkString");
				setSuperattributedescriptorpk(new Long(34308183));
				setModifiers(new Integer(1055));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(1));
				setTypepkstring(new Long(393298));
				setPk(new Long(90734679));
				setCreatedts(new Timestamp(1380205277887L));
				setModifiedts(new Timestamp(1380205288610L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("attributedescriptors");
				setQualifierinternal("attributedescriptors");
				setAttributetypepk(new Long(753747));
				setIshidden(isAttributeHidden());
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(8365));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(309723223));
				setCreatedts(new Timestamp(1380205281383L));
				setModifiedts(new Timestamp(1380205290844L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("syncjobs");
				setQualifierinternal("syncJobs");
				setAttributetypepk(new Long(5275731));
				setIshidden(isAttributeHidden());
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(157));
				setRelationname("SyncJob2TypeRel");
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setOrdered(isNotOrdered());
				setRelationtype(new Long(9240658));
				setGenerate(isGenerate());
				setIssource(isNotSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(89194583));
				setCreatedts(new Timestamp(1380205277854L));
				setModifiedts(new Timestamp(1380205303721L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("code");
				setQualifierinternal("code");
				setAttributetypepk(new Long(327761));
				setIshidden(isAttributeShown());
				setColumnname("InternalCode");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796093218898,8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("internalCode");
				setSuperattributedescriptorpk(new Long(89161815));
				setModifiers(new Integer(3093));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
				setUnique(isUnique());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(335544407));
				setCreatedts(new Timestamp(1380205281791L));
				setModifiedts(new Timestamp(1380205302142L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("comments");
				setQualifierinternal("comments");
				setAttributetypepk(new Long(6455379));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,8796093218898,8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(322568279));
				setModifiers(new Integer(1055));
				setRelationname("CommentItemRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("comments");
				setOrdered(isOrdered());
				setRelationtype(new Long(11862098));
				setGenerate(isGenerate());
				setIssource(isNotSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(286326871));
				setCreatedts(new Timestamp(1380205281028L));
				setModifiedts(new Timestamp(1380205303345L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("synchronizationsources");
				setQualifierinternal("synchronizationSources");
				setAttributetypepk(new Long(2326611));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,8796093218898,8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(273350743));
				setModifiers(new Integer(9229));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(246448215));
				setCreatedts(new Timestamp(1380205280414L));
				setModifiedts(new Timestamp(1380205306048L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("systemtype");
				setQualifierinternal("systemType");
				setAttributetypepk(new Long(131153));
				setIshidden(isAttributeShown());
				setColumnname("p_systemtype");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(131153));
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(279));
				setAutocreate(isAutoCreate());
				setExtensionname("impex");
				setGenerate(isGenerate());
				setDefaultvaluedefinitionstring("Boolean.FALSE");
				setDefaultvalue(new byte[]
						{ -84, -19, 0, 5, 115, 114, 0, 17, 106, 97, 118, 97, 46, 108, 97, 110, 103, 46, 66, 111, 111, 108, 101, 97,
								110, -51,
								32, 114, -128, -43, -100, -6, -18, 2, 0, 1, 90, 0, 5, 118, 97, 108, 117, 101, 120, 112, 0 });
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(172490839));
				setCreatedts(new Timestamp(1380205279320L));
				setModifiedts(new Timestamp(1380205303193L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("savedvalues");
				setQualifierinternal("savedValues");
				setAttributetypepk(new Long(3833939));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,8796093218898,8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(159514711));
				setModifiers(new Integer(1037));
				setRelationname("ItemSavedValuesRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setOrdered(isOrdered());
				setRelationtype(new Long(2818130));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(291274839));
				setCreatedts(new Timestamp(1380205281109L));
				setModifiedts(new Timestamp(1380205291176L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("uniquekeyattributequalifier");
				setQualifierinternal("uniqueKeyAttributeQualifier");
				setAttributetypepk(new Long(327761));
				setIshidden(isAttributeHidden());
				setColumnname("p_uniquekeyattributequalifier");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(415));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(368115799));
				setCreatedts(new Timestamp(1380205282213L));
				setModifiedts(new Timestamp(1380205306024L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("hmcicon");
				setQualifierinternal("hmcIcon");
				setAttributetypepk(new Long(1867858));
				setIshidden(isAttributeShown());
				setColumnname("p_hmcicon");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("hmc");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(192184407));
				setCreatedts(new Timestamp(1380205279671L));
				setModifiedts(new Timestamp(1380205301753L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("alldocuments");
				setQualifierinternal("allDocuments");
				setAttributetypepk(new Long(4227155));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,8796093218898,8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(179208279));
				setModifiers(new Integer(1071));
				setRelationname("ItemDocrRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("commons");
				setOrdered(isNotOrdered());
				setRelationtype(new Long(4128850));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(64421975));
				setCreatedts(new Timestamp(1380205277356L));
				setModifiedts(new Timestamp(1380205302878L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("owner");
				setQualifierinternal("owner");
				setAttributetypepk(getItemTypeCodePK());
				setIshidden(isAttributeShown());
				setColumnname("OwnerPkString");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,8796093218898,8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("ownerPkString");
				setSuperattributedescriptorpk(new Long(51445847));
				setModifiers(new Integer(3101));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(30146647));
				setCreatedts(new Timestamp(1380205276569L));
				setModifiedts(new Timestamp(1380205302688L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("modifiedtime");
				setQualifierinternal("modifiedtime");
				setAttributetypepk(new Long(393297));
				setIshidden(isAttributeShown());
				setColumnname("modifiedTS");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(393297));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,8796093218898,8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("modifiedTimestampInternal");
				setSuperattributedescriptorpk(new Long(17170519));
				setModifiers(new Integer(1045));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(91914327));
				setCreatedts(new Timestamp(1380205277908L));
				setModifiedts(new Timestamp(1380205306032L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("jaloonly");
				setQualifierinternal("jaloonly");
				setAttributetypepk(new Long(131153));
				setIshidden(isAttributeShown());
				setColumnname("p_jaloonly");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(131153));
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(279));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
				setDefaultvaluedefinitionstring("java.lang.Boolean.FALSE");
				setDefaultvalue(new byte[]
						{ -84, -19, 0, 5, 115, 114, 0, 17, 106, 97, 118, 97, 46, 108, 97, 110, 103, 46, 66, 111, 111, 108, 101, 97,
								110, -51,
								32, 114, -128, -43, -100, -6, -18, 2, 0, 1, 90, 0, 5, 118, 97, 108, 117, 101, 120, 112, 0 });
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(92700759));
				setCreatedts(new Timestamp(1380205277922L));
				setModifiedts(new Timestamp(1380205306050L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("table");
				setQualifierinternal("table");
				setAttributetypepk(new Long(327761));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(8205));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(86999127));
				setCreatedts(new Timestamp(1380205277820L));
				setModifiedts(new Timestamp(1380205303694L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("deprecated");
				setQualifierinternal("deprecated");
				setAttributetypepk(new Long(131153));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(131153));
				setInheritancepathstring(",8796093186130,8796093218898,8796093251666,");
				setIsproperty(isAttributeProperty());
				setSuperattributedescriptorpk(new Long(86966359));
				setModifiers(new Integer(9489));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(91324503));
				setCreatedts(new Timestamp(1380205277897L));
				setModifiedts(new Timestamp(1380205306029L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("jaloclass");
				setQualifierinternal("jaloclass");
				setAttributetypepk(new Long(458833));
				setIshidden(isAttributeShown());
				setColumnname("jaloClassName");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(458833));
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("jaloClassName");
				setModifiers(new Integer(31));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(90144855));
				setCreatedts(new Timestamp(1380205277876L));
				setModifiedts(new Timestamp(1380205305997L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("abstract");
				setQualifierinternal("abstract");
				setAttributetypepk(new Long(131153));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(8197));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(291078231));
				setCreatedts(new Timestamp(1380205281106L));
				setModifiedts(new Timestamp(1380205306054L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("uniquekeyattributes");
				setQualifierinternal("uniqueKeyAttributes");
				setAttributetypepk(new Long(753747));
				setSelectiondescriptorpk(new Long(90734679));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(8207));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(92897367));
				setCreatedts(new Timestamp(1380205277926L));
				setModifiedts(new Timestamp(1380205306002L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("allsupertypes");
				setQualifierinternal("allSuperTypes");
				setAttributetypepk(new Long(720979));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(8201));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(85753943));
				setCreatedts(new Timestamp(1380205277800L));
				setModifiedts(new Timestamp(1380205303713L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("name");
				setQualifierinternal("name");
				setAttributetypepk(new Long(65620));
				setIshidden(isAttributeShown());
				setColumnname("p_name");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796093186130,8796093218898,8796093251666,");
				setIsproperty(isAttributeProperty());
				setSuperattributedescriptorpk(new Long(85721175));
				setModifiers(new Integer(1823));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(13008983));
				setCreatedts(new Timestamp(1380205276054L));
				setModifiedts(new Timestamp(1380205302327L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("creationtime");
				setQualifierinternal("creationtime");
				setAttributetypepk(new Long(393297));
				setIshidden(isAttributeShown());
				setColumnname("createdTS");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(393297));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,8796093218898,8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("creationTimestampInternal");
				setSuperattributedescriptorpk(new Long(32855));
				setModifiers(new Integer(3101));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(4));
				setTypepkstring(new Long(655442));
				setPk(new Long(364544087));
				setCreatedts(new Timestamp(1380205282160L));
				setModifiedts(new Timestamp(1380205306010L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("cockpititemtemplates");
				setQualifierinternal("cockpitItemTemplates");
				setAttributetypepk(new Long(7372883));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(15));
				setRelationname("CockpitItemTemplate2ComposedTypeRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("cockpit");
				setOrdered(isNotOrdered());
				setRelationtype(new Long(13828178));
				setGenerate(isGenerate());
				setIssource(isNotSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(290881623));
				setCreatedts(new Timestamp(1380205281103L));
				setModifiedts(new Timestamp(1380205291172L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("catalogversionattributequalifier");
				setQualifierinternal("catalogVersionAttributeQualifier");
				setAttributetypepk(new Long(327761));
				setIshidden(isAttributeHidden());
				setColumnname("p_catalogversionattributequali");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(415));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(93290583));
				setCreatedts(new Timestamp(1380205277933L));
				setModifiedts(new Timestamp(1380205306036L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("legacypersistence");
				setQualifierinternal("legacyPersistence");
				setAttributetypepk(new Long(131153));
				setIshidden(isAttributeShown());
				setColumnname("p_legacypersistence");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(131153));
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
				setDefaultvaluedefinitionstring("Boolean.FALSE");
				setDefaultvalue(new byte[]
						{ -84, -19, 0, 5, 115, 114, 0, 17, 106, 97, 118, 97, 46, 108, 97, 110, 103, 46, 66, 111, 111, 108, 101, 97,
								110, -51,
								32, 114, -128, -43, -100, -6, -18, 2, 0, 1, 90, 0, 5, 118, 97, 108, 117, 101, 120, 112, 0 });
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(88866903));
				setCreatedts(new Timestamp(1380205277848L));
				setModifiedts(new Timestamp(1380205303732L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("xmldefinition");
				setQualifierinternal("xmldefinition");
				setAttributetypepk(new Long(327761));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setInheritancepathstring(",8796093218898,8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(88834135));
				setModifiers(new Integer(9229));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(4));
				setTypepkstring(new Long(655442));
				setPk(new Long(178716759));
				setCreatedts(new Timestamp(1380205279467L));
				setModifiedts(new Timestamp(1380205306022L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("formats");
				setQualifierinternal("formats");
				setAttributetypepk(new Long(4096083));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(31));
				setRelationname("Format2ComTypRel");
				setAutocreate(isAutoCreate());
				setExtensionname("commons");
				setOrdered(isOrdered());
				setRelationtype(new Long(3997778));
				setGenerate(isGenerate());
				setIssource(isNotSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(92504151));
				setCreatedts(new Timestamp(1380205277919L));
				setModifiedts(new Timestamp(1380205306046L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("supertype");
				setQualifierinternal("superType");
				setAttributetypepk(getComposedTypeTypeCodePK());
				setIshidden(isAttributeShown());
				setColumnname("SuperTypePK");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("superTypePK");
				setModifiers(new Integer(2069));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(87621719));
				setCreatedts(new Timestamp(1380205277829L));
				setModifiedts(new Timestamp(1380205303688L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("autocreate");
				setQualifierinternal("autocreate");
				setAttributetypepk(new Long(131153));
				setIshidden(isAttributeShown());
				setColumnname("p_autocreate");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(131153));
				setInheritancepathstring(",8796093186130,8796093218898,8796093251666,");
				setIsproperty(isAttributeProperty());
				setSuperattributedescriptorpk(new Long(87588951));
				setModifiers(new Integer(1299));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
				setDefaultvaluedefinitionstring("java.lang.Boolean.TRUE");
				setDefaultvalue(new byte[]
						{ -84, -19, 0, 5, 115, 114, 0, 17, 106, 97, 118, 97, 46, 108, 97, 110, 103, 46, 66, 111, 111, 108, 101, 97,
								110, -51,
								32, 114, -128, -43, -100, -6, -18, 2, 0, 1, 90, 0, 5, 118, 97, 108, 117, 101, 120, 112, 1 });
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(91127895));
				setCreatedts(new Timestamp(1380205277894L));
				setModifiedts(new Timestamp(1380205306027L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("inheritedattributedescriptors");
				setQualifierinternal("inheritedattributedescriptors");
				setAttributetypepk(new Long(753747));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(8237));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(91717719));
				setCreatedts(new Timestamp(1380205277904L));
				setModifiedts(new Timestamp(1380205306041L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("singleton");
				setQualifierinternal("singleton");
				setAttributetypepk(new Long(131153));
				setIshidden(isAttributeShown());
				setColumnname("Singleton");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(131153));
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("singletonFlag");
				setModifiers(new Integer(23));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(290685015));
				setCreatedts(new Timestamp(1380205281101L));
				setModifiedts(new Timestamp(1380205306008L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("catalogversionattribute");
				setQualifierinternal("catalogVersionAttribute");
				setAttributetypepk(new Long(393298));
				setSelectiondescriptorpk(new Long(90734679));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(8207));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(81559639));
				setCreatedts(new Timestamp(1380205277705L));
				setModifiedts(new Timestamp(1380205303035L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("pk");
				setQualifierinternal("pk");
				setAttributetypepk(new Long(622673));
				setIshidden(isAttributeShown());
				setColumnname("PK");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(622673));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,8796093218898,8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("pkString");
				setSuperattributedescriptorpk(new Long(68583511));
				setModifiers(new Integer(1045));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(89522263));
				setCreatedts(new Timestamp(1380205277860L));
				setModifiedts(new Timestamp(1380205303724L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("defaultvalue");
				setQualifierinternal("defaultValue");
				setAttributetypepk(new Long(32849));
				setIshidden(isAttributeShown());
				setColumnname("p_defaultvalue");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(32849));
				setInheritancepathstring(",8796093218898,8796093251666,");
				setIsproperty(isAttributeProperty());
				setSuperattributedescriptorpk(new Long(89489495));
				setModifiers(new Integer(1311));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(1));
				setTypepkstring(new Long(393298));
				setPk(new Long(90931287));
				setCreatedts(new Timestamp(1380205277890L));
				setModifiedts(new Timestamp(1380205288611L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("inheritancepathstring");
				setQualifierinternal("inheritancePathString");
				setAttributetypepk(new Long(327761));
				setIshidden(isAttributeHidden());
				setColumnname("InheritancePathString");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("inheritancePathStringInternal");
				setModifiers(new Integer(157));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(368312407));
				setCreatedts(new Timestamp(1380205282215L));
				setModifiedts(new Timestamp(1380205306039L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("loghmcchanges");
				setQualifierinternal("logHMCChanges");
				setAttributetypepk(new Long(131153));
				setIshidden(isAttributeShown());
				setColumnname("p_loghmcchanges");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(131153));
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(279));
				setAutocreate(isAutoCreate());
				setExtensionname("hmc");
				setGenerate(isGenerate());
				setDefaultvaluedefinitionstring("java.lang.Boolean.TRUE");
				setDefaultvalue(new byte[]
						{ -84, -19, 0, 5, 115, 114, 0, 17, 106, 97, 118, 97, 46, 108, 97, 110, 103, 46, 66, 111, 111, 108, 101, 97,
								110, -51,
								32, 114, -128, -43, -100, -6, -18, 2, 0, 1, 90, 0, 5, 118, 97, 108, 117, 101, 120, 112, 1 });
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(290488407));
				setCreatedts(new Timestamp(1380205281098L));
				setModifiedts(new Timestamp(1380205306005L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("catalogitemtype");
				setQualifierinternal("catalogItemType");
				setAttributetypepk(new Long(131153));
				setIshidden(isAttributeShown());
				setColumnname("p_catalogitemtype");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(131153));
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(279));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(47284311));
				setCreatedts(new Timestamp(1380205277000L));
				setModifiedts(new Timestamp(1380205302520L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("itemtype");
				setQualifierinternal("itemtype");
				setAttributetypepk(getComposedTypeTypeCodePK());
				setIshidden(isAttributeShown());
				setColumnname("TypePkString");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,8796093218898,8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("typePkString");
				setSuperattributedescriptorpk(new Long(34308183));
				setModifiers(new Integer(1055));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(90341463));
				setCreatedts(new Timestamp(1380205277880L));
				setModifiedts(new Timestamp(1380205306015L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("declaredattributedescriptors");
				setQualifierinternal("declaredattributedescriptors");
				setAttributetypepk(new Long(753747));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(8239));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(90538071));
				setCreatedts(new Timestamp(1380205277884L));
				setModifiedts(new Timestamp(1380205306017L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("dumppropertytable");
				setQualifierinternal("dumpPropertyTable");
				setAttributetypepk(new Long(327761));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(8205));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(92110935));
				setCreatedts(new Timestamp(1380205277912L));
				setModifiedts(new Timestamp(1380205306020L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("dynamic");
				setQualifierinternal("dynamic");
				setAttributetypepk(new Long(131153));
				setIshidden(isAttributeShown());
				setColumnname("p_dynamic");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(131153));
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(277));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
				setDefaultvaluedefinitionstring("java.lang.Boolean.FALSE");
				setDefaultvalue(new byte[]
						{ -84, -19, 0, 5, 115, 114, 0, 17, 106, 97, 118, 97, 46, 108, 97, 110, 103, 46, 66, 111, 111, 108, 101, 97,
								110, -51,
								32, 114, -128, -43, -100, -6, -18, 2, 0, 1, 90, 0, 5, 118, 97, 108, 117, 101, 120, 112, 0 });
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(88244311));
				setCreatedts(new Timestamp(1380205277839L));
				setModifiedts(new Timestamp(1380205303706L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("generate");
				setQualifierinternal("generate");
				setAttributetypepk(new Long(131153));
				setIshidden(isAttributeShown());
				setColumnname("p_generate");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(131153));
				setInheritancepathstring(",8796093186130,8796093218898,8796093251666,");
				setIsproperty(isAttributeProperty());
				setSuperattributedescriptorpk(new Long(88211543));
				setModifiers(new Integer(1299));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(93093975));
				setCreatedts(new Timestamp(1380205277929L));
				setModifiedts(new Timestamp(1380205306000L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("allsubtypes");
				setQualifierinternal("allSubTypes");
				setAttributetypepk(new Long(720979));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(8201));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(89849943));
				setCreatedts(new Timestamp(1380205277868L));
				setModifiedts(new Timestamp(1380205303728L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("description");
				setQualifierinternal("description");
				setAttributetypepk(new Long(65620));
				setIshidden(isAttributeShown());
				setColumnname("p_description");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796093218898,8796093251666,");
				setIsproperty(isAttributeProperty());
				setSuperattributedescriptorpk(new Long(89817175));
				setModifiers(new Integer(1823));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(92307543));
				setCreatedts(new Timestamp(1380205277915L));
				setModifiedts(new Timestamp(1380205306043L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("subtypes");
				setQualifierinternal("subtypes");
				setAttributetypepk(new Long(884819));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(8205));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(269189207));
				setCreatedts(new Timestamp(1380205280786L));
				setModifiedts(new Timestamp(1380205303492L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("synchronizedcopies");
				setQualifierinternal("synchronizedCopies");
				setAttributetypepk(new Long(2326611));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,8796093218898,8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(256213079));
				setModifiers(new Integer(9229));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(360349783));
				setCreatedts(new Timestamp(1380205282106L));
				setModifiedts(new Timestamp(1380205301953L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("assignedcockpititemtemplates");
				setQualifierinternal("assignedCockpitItemTemplates");
				setAttributetypepk(new Long(7340115));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,8796093218898,8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(347373655));
				setModifiers(new Integer(1055));
				setRelationname("Item2CockpitItemTemplateRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("cockpit");
				setOrdered(isOrdered());
				setRelationtype(new Long(13795410));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(4));
				setTypepkstring(new Long(655442));
				setPk(new Long(254410839));
				setCreatedts(new Timestamp(1380205280558L));
				setModifiedts(new Timestamp(1380205306012L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("constraints");
				setQualifierinternal("constraints");
				setAttributetypepk(new Long(4718675));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(15));
				setRelationname("ConstraintCompositeTypeRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("validation");
				setOrdered(isNotOrdered());
				setRelationtype(new Long(7405650));
				setGenerate(isGenerate());
				setIssource(isNotSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(86376535));
				setCreatedts(new Timestamp(1380205277811L));
				setModifiedts(new Timestamp(1380205303700L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("extensionname");
				setQualifierinternal("extensionName");
				setAttributetypepk(new Long(327761));
				setIshidden(isAttributeShown());
				setColumnname("p_extensionname");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796093186130,8796093218898,8796093251666,");
				setIsproperty(isAttributeProperty());
				setSuperattributedescriptorpk(new Long(86343767));
				setModifiers(new Integer(1307));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(91521111));
				setCreatedts(new Timestamp(1380205277900L));
				setModifiedts(new Timestamp(1380205306034L));
				setOwnerpkstring(getComposedTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("jndiname");
				setQualifierinternal("jndiName");
				setAttributetypepk(new Long(327761));
				setIshidden(isAttributeShown());
				setColumnname("ItemJNDIName");
				setEnclosingtypepk(getComposedTypeTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796093251666,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("itemJNDIName");
				setModifiers(new Integer(29));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(33423447));
				setCreatedts(new Timestamp(1380205276646L));
				setModifiedts(new Timestamp(1380205302698L));
				setOwnerpkstring(getLocalizedItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("modifiedtime");
				setQualifierinternal("modifiedtime");
				setAttributetypepk(new Long(393297));
				setIshidden(isAttributeShown());
				setColumnname("modifiedTS");
				setEnclosingtypepk(getLocalizedItemTypeCodePK());
				setPersistencetypepk(new Long(393297));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("modifiedTimestampInternal");
				setSuperattributedescriptorpk(new Long(17170519));
				setModifiers(new Integer(1045));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(16285783));
				setCreatedts(new Timestamp(1380205276156L));
				setModifiedts(new Timestamp(1380205302338L));
				setOwnerpkstring(getLocalizedItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("creationtime");
				setQualifierinternal("creationtime");
				setAttributetypepk(new Long(393297));
				setIshidden(isAttributeShown());
				setColumnname("createdTS");
				setEnclosingtypepk(getLocalizedItemTypeCodePK());
				setPersistencetypepk(new Long(393297));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("creationTimestampInternal");
				setSuperattributedescriptorpk(new Long(32855));
				setModifiers(new Integer(3101));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(84836439));
				setCreatedts(new Timestamp(1380205277781L));
				setModifiedts(new Timestamp(1380205303045L));
				setOwnerpkstring(getLocalizedItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("pk");
				setQualifierinternal("pk");
				setAttributetypepk(new Long(622673));
				setIshidden(isAttributeShown());
				setColumnname("PK");
				setEnclosingtypepk(getLocalizedItemTypeCodePK());
				setPersistencetypepk(new Long(622673));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("pkString");
				setSuperattributedescriptorpk(new Long(68583511));
				setModifiers(new Integer(1045));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(175767639));
				setCreatedts(new Timestamp(1380205279369L));
				setModifiedts(new Timestamp(1380205303203L));
				setOwnerpkstring(getLocalizedItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("savedvalues");
				setQualifierinternal("savedValues");
				setAttributetypepk(new Long(3833939));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getLocalizedItemTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(159514711));
				setModifiers(new Integer(1037));
				setRelationname("ItemSavedValuesRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setOrdered(isOrdered());
				setRelationtype(new Long(2818130));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(289603671));
				setCreatedts(new Timestamp(1380205281083L));
				setModifiedts(new Timestamp(1380205303355L));
				setOwnerpkstring(getLocalizedItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("synchronizationsources");
				setQualifierinternal("synchronizationSources");
				setAttributetypepk(new Long(2326611));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getLocalizedItemTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(273350743));
				setModifiers(new Integer(9229));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(67698775));
				setCreatedts(new Timestamp(1380205277428L));
				setModifiedts(new Timestamp(1380205302888L));
				setOwnerpkstring(getLocalizedItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("owner");
				setQualifierinternal("owner");
				setAttributetypepk(getItemTypeCodePK());
				setIshidden(isAttributeShown());
				setColumnname("OwnerPkString");
				setEnclosingtypepk(getLocalizedItemTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("ownerPkString");
				setSuperattributedescriptorpk(new Long(51445847));
				setModifiers(new Integer(3101));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(338821207));
				setCreatedts(new Timestamp(1380205281830L));
				setModifiedts(new Timestamp(1380205302153L));
				setOwnerpkstring(getLocalizedItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("comments");
				setQualifierinternal("comments");
				setAttributetypepk(new Long(6455379));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getLocalizedItemTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(322568279));
				setModifiers(new Integer(1055));
				setRelationname("CommentItemRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("comments");
				setOrdered(isOrdered());
				setRelationtype(new Long(11862098));
				setGenerate(isGenerate());
				setIssource(isNotSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(272466007));
				setCreatedts(new Timestamp(1380205280834L));
				setModifiedts(new Timestamp(1380205303503L));
				setOwnerpkstring(getLocalizedItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("synchronizedcopies");
				setQualifierinternal("synchronizedCopies");
				setAttributetypepk(new Long(2326611));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getLocalizedItemTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(256213079));
				setModifiers(new Integer(9229));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(363626583));
				setCreatedts(new Timestamp(1380205282147L));
				setModifiedts(new Timestamp(1380205301967L));
				setOwnerpkstring(getLocalizedItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("assignedcockpititemtemplates");
				setQualifierinternal("assignedCockpitItemTemplates");
				setAttributetypepk(new Long(7340115));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getLocalizedItemTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(347373655));
				setModifiers(new Integer(1055));
				setRelationname("Item2CockpitItemTemplateRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("cockpit");
				setOrdered(isOrdered());
				setRelationtype(new Long(13795410));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(195461207));
				setCreatedts(new Timestamp(1380205279719L));
				setModifiedts(new Timestamp(1380205301767L));
				setOwnerpkstring(getLocalizedItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("alldocuments");
				setQualifierinternal("allDocuments");
				setAttributetypepk(new Long(4227155));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getLocalizedItemTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(179208279));
				setModifiers(new Integer(1071));
				setRelationname("ItemDocrRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("commons");
				setOrdered(isNotOrdered());
				setRelationtype(new Long(4128850));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(50561111));
				setCreatedts(new Timestamp(1380205277070L));
				setModifiedts(new Timestamp(1380205302530L));
				setOwnerpkstring(getLocalizedItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("itemtype");
				setQualifierinternal("itemtype");
				setAttributetypepk(getComposedTypeTypeCodePK());
				setIshidden(isAttributeShown());
				setColumnname("TypePkString");
				setEnclosingtypepk(getLocalizedItemTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("typePkString");
				setSuperattributedescriptorpk(new Long(34308183));
				setModifiers(new Integer(1055));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(335937623));
				setCreatedts(new Timestamp(1380205281796L));
				setModifiedts(new Timestamp(1380205302143L));
				setOwnerpkstring(getTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("comments");
				setQualifierinternal("comments");
				setAttributetypepk(new Long(6455379));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getTypeTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,8796093218898,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(322568279));
				setModifiers(new Integer(1055));
				setRelationname("CommentItemRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("comments");
				setOrdered(isOrdered());
				setRelationtype(new Long(11862098));
				setGenerate(isGenerate());
				setIssource(isNotSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(286720087));
				setCreatedts(new Timestamp(1380205281034L));
				setModifiedts(new Timestamp(1380205303347L));
				setOwnerpkstring(getTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("synchronizationsources");
				setQualifierinternal("synchronizationSources");
				setAttributetypepk(new Long(2326611));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getTypeTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,8796093218898,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(273350743));
				setModifiers(new Integer(9229));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(86933591));
				setCreatedts(new Timestamp(1380205277819L));
				setModifiedts(new Timestamp(1380205303701L));
				setOwnerpkstring(getTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("extensionname");
				setQualifierinternal("extensionName");
				setAttributetypepk(new Long(327761));
				setIshidden(isAttributeShown());
				setColumnname("p_extensionname");
				setEnclosingtypepk(getTypeTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796093186130,8796093218898,");
				setIsproperty(isAttributeProperty());
				setSuperattributedescriptorpk(new Long(86343767));
				setModifiers(new Integer(1307));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(88178775));
				setCreatedts(new Timestamp(1380205277837L));
				setModifiedts(new Timestamp(1380205303690L));
				setOwnerpkstring(getTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("autocreate");
				setQualifierinternal("autocreate");
				setAttributetypepk(new Long(131153));
				setIshidden(isAttributeShown());
				setColumnname("p_autocreate");
				setEnclosingtypepk(getTypeTypeCodePK());
				setPersistencetypepk(new Long(131153));
				setInheritancepathstring(",8796093186130,8796093218898,");
				setIsproperty(isAttributeProperty());
				setSuperattributedescriptorpk(new Long(87588951));
				setModifiers(new Integer(1299));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
				setDefaultvaluedefinitionstring("java.lang.Boolean.TRUE");
				setDefaultvalue(new byte[]
						{ -84, -19, 0, 5, 115, 114, 0, 17, 106, 97, 118, 97, 46, 108, 97, 110, 103, 46, 66, 111, 111, 108, 101, 97,
								110, -51,
								32, 114, -128, -43, -100, -6, -18, 2, 0, 1, 90, 0, 5, 118, 97, 108, 117, 101, 120, 112, 1 });
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(88834135));
				setCreatedts(new Timestamp(1380205277847L));
				setModifiedts(new Timestamp(1380205303732L));
				setOwnerpkstring(getTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("xmldefinition");
				setQualifierinternal("xmldefinition");
				setAttributetypepk(new Long(327761));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getTypeTypeCodePK());
				setInheritancepathstring(",8796093218898,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(8205));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(4));
				setTypepkstring(new Long(393298));
				setPk(new Long(89817175));
				setCreatedts(new Timestamp(1380205277864L));
				setModifiedts(new Timestamp(1380205303728L));
				setOwnerpkstring(getTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("description");
				setQualifierinternal("description");
				setAttributetypepk(new Long(65620));
				setIshidden(isAttributeShown());
				setColumnname("p_description");
				setEnclosingtypepk(getTypeTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796093218898,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(799));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(89489495));
				setCreatedts(new Timestamp(1380205277859L));
				setModifiedts(new Timestamp(1380205303724L));
				setOwnerpkstring(getTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("defaultvalue");
				setQualifierinternal("defaultValue");
				setAttributetypepk(new Long(32849));
				setIshidden(isAttributeShown());
				setColumnname("p_defaultvalue");
				setEnclosingtypepk(getTypeTypeCodePK());
				setPersistencetypepk(new Long(32849));
				setInheritancepathstring(",8796093218898,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(287));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(30539863));
				setCreatedts(new Timestamp(1380205276578L));
				setModifiedts(new Timestamp(1380205302689L));
				setOwnerpkstring(getTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("modifiedtime");
				setQualifierinternal("modifiedtime");
				setAttributetypepk(new Long(393297));
				setIshidden(isAttributeShown());
				setColumnname("modifiedTS");
				setEnclosingtypepk(getTypeTypeCodePK());
				setPersistencetypepk(new Long(393297));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,8796093218898,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("modifiedTimestampInternal");
				setSuperattributedescriptorpk(new Long(17170519));
				setModifiers(new Integer(1045));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(360742999));
				setCreatedts(new Timestamp(1380205282111L));
				setModifiedts(new Timestamp(1380205301955L));
				setOwnerpkstring(getTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("assignedcockpititemtemplates");
				setQualifierinternal("assignedCockpitItemTemplates");
				setAttributetypepk(new Long(7340115));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getTypeTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,8796093218898,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(347373655));
				setModifiers(new Integer(1055));
				setRelationname("Item2CockpitItemTemplateRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("cockpit");
				setOrdered(isOrdered());
				setRelationtype(new Long(13795410));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(192577623));
				setCreatedts(new Timestamp(1380205279677L));
				setModifiedts(new Timestamp(1380205301755L));
				setOwnerpkstring(getTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("alldocuments");
				setQualifierinternal("allDocuments");
				setAttributetypepk(new Long(4227155));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getTypeTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,8796093218898,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(179208279));
				setModifiers(new Integer(1071));
				setRelationname("ItemDocrRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("commons");
				setOrdered(isNotOrdered());
				setRelationtype(new Long(4128850));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(47677527));
				setCreatedts(new Timestamp(1380205277008L));
				setModifiedts(new Timestamp(1380205302521L));
				setOwnerpkstring(getTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("itemtype");
				setQualifierinternal("itemtype");
				setAttributetypepk(getComposedTypeTypeCodePK());
				setIshidden(isAttributeShown());
				setColumnname("TypePkString");
				setEnclosingtypepk(getTypeTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,8796093218898,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("typePkString");
				setSuperattributedescriptorpk(new Long(34308183));
				setModifiers(new Integer(1055));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(87556183));
				setCreatedts(new Timestamp(1380205277828L));
				setModifiedts(new Timestamp(1380205303696L));
				setOwnerpkstring(getTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("deprecated");
				setQualifierinternal("deprecated");
				setAttributetypepk(new Long(131153));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getTypeTypeCodePK());
				setPersistencetypepk(new Long(131153));
				setInheritancepathstring(",8796093186130,8796093218898,");
				setIsproperty(isAttributeProperty());
				setSuperattributedescriptorpk(new Long(86966359));
				setModifiers(new Integer(9489));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(172884055));
				setCreatedts(new Timestamp(1380205279326L));
				setModifiedts(new Timestamp(1380205303194L));
				setOwnerpkstring(getTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("savedvalues");
				setQualifierinternal("savedValues");
				setAttributetypepk(new Long(3833939));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getTypeTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,8796093218898,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(159514711));
				setModifiers(new Integer(1037));
				setRelationname("ItemSavedValuesRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setOrdered(isOrdered());
				setRelationtype(new Long(2818130));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(86310999));
				setCreatedts(new Timestamp(1380205277809L));
				setModifiedts(new Timestamp(1380205303714L));
				setOwnerpkstring(getTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("name");
				setQualifierinternal("name");
				setAttributetypepk(new Long(65620));
				setIshidden(isAttributeShown());
				setColumnname("p_name");
				setEnclosingtypepk(getTypeTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796093186130,8796093218898,");
				setIsproperty(isAttributeProperty());
				setSuperattributedescriptorpk(new Long(85721175));
				setModifiers(new Integer(1823));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(64815191));
				setCreatedts(new Timestamp(1380205277363L));
				setModifiedts(new Timestamp(1380205302879L));
				setOwnerpkstring(getTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("owner");
				setQualifierinternal("owner");
				setAttributetypepk(getItemTypeCodePK());
				setIshidden(isAttributeShown());
				setColumnname("OwnerPkString");
				setEnclosingtypepk(getTypeTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,8796093218898,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("ownerPkString");
				setSuperattributedescriptorpk(new Long(51445847));
				setModifiers(new Integer(3101));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(269582423));
				setCreatedts(new Timestamp(1380205280792L));
				setModifiedts(new Timestamp(1380205303493L));
				setOwnerpkstring(getTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("synchronizedcopies");
				setQualifierinternal("synchronizedCopies");
				setAttributetypepk(new Long(2326611));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getTypeTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,8796093218898,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(256213079));
				setModifiers(new Integer(9229));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(81952855));
				setCreatedts(new Timestamp(1380205277714L));
				setModifiedts(new Timestamp(1380205303036L));
				setOwnerpkstring(getTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("pk");
				setQualifierinternal("pk");
				setAttributetypepk(new Long(622673));
				setIshidden(isAttributeShown());
				setColumnname("PK");
				setEnclosingtypepk(getTypeTypeCodePK());
				setPersistencetypepk(new Long(622673));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,8796093218898,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("pkString");
				setSuperattributedescriptorpk(new Long(68583511));
				setModifiers(new Integer(1045));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(13402199));
				setCreatedts(new Timestamp(1380205276069L));
				setModifiedts(new Timestamp(1380205302328L));
				setOwnerpkstring(getTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("creationtime");
				setQualifierinternal("creationtime");
				setAttributetypepk(new Long(393297));
				setIshidden(isAttributeShown());
				setColumnname("createdTS");
				setEnclosingtypepk(getTypeTypeCodePK());
				setPersistencetypepk(new Long(393297));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,8796093218898,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("creationTimestampInternal");
				setSuperattributedescriptorpk(new Long(32855));
				setModifiers(new Integer(3101));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(89161815));
				setCreatedts(new Timestamp(1380205277852L));
				setModifiedts(new Timestamp(1380205303720L));
				setOwnerpkstring(getTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("code");
				setQualifierinternal("code");
				setAttributetypepk(new Long(327761));
				setIshidden(isAttributeShown());
				setColumnname("InternalCode");
				setEnclosingtypepk(getTypeTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796093218898,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("internalCode");
				setModifiers(new Integer(2069));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
				setUnique(isUnique());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(88801367));
				setCreatedts(new Timestamp(1380205277847L));
				setModifiedts(new Timestamp(1380205303707L));
				setOwnerpkstring(getTypeTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("generate");
				setQualifierinternal("generate");
				setAttributetypepk(new Long(131153));
				setIshidden(isAttributeShown());
				setColumnname("p_generate");
				setEnclosingtypepk(getTypeTypeCodePK());
				setPersistencetypepk(new Long(131153));
				setInheritancepathstring(",8796093186130,8796093218898,");
				setIsproperty(isAttributeProperty());
				setSuperattributedescriptorpk(new Long(88211543));
				setModifiers(new Integer(1299));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(357826647));
				setCreatedts(new Timestamp(1380205282077L));
				setModifiedts(new Timestamp(1380205301942L));
				setOwnerpkstring(getExtensibleItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("assignedcockpititemtemplates");
				setQualifierinternal("assignedCockpitItemTemplates");
				setAttributetypepk(new Long(7340115));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getExtensibleItemTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(347373655));
				setModifiers(new Integer(1055));
				setRelationname("Item2CockpitItemTemplateRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("cockpit");
				setOrdered(isOrdered());
				setRelationtype(new Long(13795410));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(283803735));
				setCreatedts(new Timestamp(1380205280989L));
				setModifiedts(new Timestamp(1380205303338L));
				setOwnerpkstring(getExtensibleItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("synchronizationsources");
				setQualifierinternal("synchronizationSources");
				setAttributetypepk(new Long(2326611));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getExtensibleItemTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(273350743));
				setModifiers(new Integer(9229));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(44761175));
				setCreatedts(new Timestamp(1380205276946L));
				setModifiedts(new Timestamp(1380205302510L));
				setOwnerpkstring(getExtensibleItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("itemtype");
				setQualifierinternal("itemtype");
				setAttributetypepk(getComposedTypeTypeCodePK());
				setIshidden(isAttributeShown());
				setColumnname("TypePkString");
				setEnclosingtypepk(getExtensibleItemTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796093055058,8796093087826,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("typePkString");
				setSuperattributedescriptorpk(new Long(34308183));
				setModifiers(new Integer(1055));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(333021271));
				setCreatedts(new Timestamp(1380205281761L));
				setModifiedts(new Timestamp(1380205302133L));
				setOwnerpkstring(getExtensibleItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("comments");
				setQualifierinternal("comments");
				setAttributetypepk(new Long(6455379));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getExtensibleItemTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(322568279));
				setModifiers(new Integer(1055));
				setRelationname("CommentItemRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("comments");
				setOrdered(isOrdered());
				setRelationtype(new Long(11862098));
				setGenerate(isGenerate());
				setIssource(isNotSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(79036503));
				setCreatedts(new Timestamp(1380205277658L));
				setModifiedts(new Timestamp(1380205303027L));
				setOwnerpkstring(getExtensibleItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("pk");
				setQualifierinternal("pk");
				setAttributetypepk(new Long(622673));
				setIshidden(isAttributeShown());
				setColumnname("PK");
				setEnclosingtypepk(getExtensibleItemTypeCodePK());
				setPersistencetypepk(new Long(622673));
				setInheritancepathstring(",8796093055058,8796093087826,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("pkString");
				setSuperattributedescriptorpk(new Long(68583511));
				setModifiers(new Integer(1045));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(10485847));
				setCreatedts(new Timestamp(1380205275952L));
				setModifiedts(new Timestamp(1380205302319L));
				setOwnerpkstring(getExtensibleItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("creationtime");
				setQualifierinternal("creationtime");
				setAttributetypepk(new Long(393297));
				setIshidden(isAttributeShown());
				setColumnname("createdTS");
				setEnclosingtypepk(getExtensibleItemTypeCodePK());
				setPersistencetypepk(new Long(393297));
				setInheritancepathstring(",8796093055058,8796093087826,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("creationTimestampInternal");
				setSuperattributedescriptorpk(new Long(32855));
				setModifiers(new Integer(3101));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(61898839));
				setCreatedts(new Timestamp(1380205277307L));
				setModifiedts(new Timestamp(1380205302870L));
				setOwnerpkstring(getExtensibleItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("owner");
				setQualifierinternal("owner");
				setAttributetypepk(getItemTypeCodePK());
				setIshidden(isAttributeShown());
				setColumnname("OwnerPkString");
				setEnclosingtypepk(getExtensibleItemTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796093055058,8796093087826,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("ownerPkString");
				setSuperattributedescriptorpk(new Long(51445847));
				setModifiers(new Integer(3101));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(169967703));
				setCreatedts(new Timestamp(1380205279284L));
				setModifiedts(new Timestamp(1380205303185L));
				setOwnerpkstring(getExtensibleItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("savedvalues");
				setQualifierinternal("savedValues");
				setAttributetypepk(new Long(3833939));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getExtensibleItemTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(159514711));
				setModifiers(new Integer(1037));
				setRelationname("ItemSavedValuesRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setOrdered(isOrdered());
				setRelationtype(new Long(2818130));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(189661271));
				setCreatedts(new Timestamp(1380205279635L));
				setModifiedts(new Timestamp(1380205301743L));
				setOwnerpkstring(getExtensibleItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("alldocuments");
				setQualifierinternal("allDocuments");
				setAttributetypepk(new Long(4227155));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getExtensibleItemTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(179208279));
				setModifiers(new Integer(1071));
				setRelationname("ItemDocrRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("commons");
				setOrdered(isNotOrdered());
				setRelationtype(new Long(4128850));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(266666071));
				setCreatedts(new Timestamp(1380205280734L));
				setModifiedts(new Timestamp(1380205303483L));
				setOwnerpkstring(getExtensibleItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("synchronizedcopies");
				setQualifierinternal("synchronizedCopies");
				setAttributetypepk(new Long(2326611));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getExtensibleItemTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(256213079));
				setModifiers(new Integer(9229));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(27623511));
				setCreatedts(new Timestamp(1380205276505L));
				setModifiedts(new Timestamp(1380205302679L));
				setOwnerpkstring(getExtensibleItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("modifiedtime");
				setQualifierinternal("modifiedtime");
				setAttributetypepk(new Long(393297));
				setIshidden(isAttributeShown());
				setColumnname("modifiedTS");
				setEnclosingtypepk(getExtensibleItemTypeCodePK());
				setPersistencetypepk(new Long(393297));
				setInheritancepathstring(",8796093055058,8796093087826,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("modifiedTimestampInternal");
				setSuperattributedescriptorpk(new Long(17170519));
				setModifiers(new Integer(1045));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(282820695));
				setCreatedts(new Timestamp(1380205280975L));
				setModifiedts(new Timestamp(1380205303335L));
				setOwnerpkstring(getTypeManagerManagedTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("synchronizationsources");
				setQualifierinternal("synchronizationSources");
				setAttributetypepk(new Long(2326611));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getTypeManagerManagedTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(273350743));
				setModifiers(new Integer(9229));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(332038231));
				setCreatedts(new Timestamp(1380205281735L));
				setModifiedts(new Timestamp(1380205302130L));
				setOwnerpkstring(getTypeManagerManagedTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("comments");
				setQualifierinternal("comments");
				setAttributetypepk(new Long(6455379));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getTypeManagerManagedTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(322568279));
				setModifiers(new Integer(1055));
				setRelationname("CommentItemRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("comments");
				setOrdered(isOrdered());
				setRelationtype(new Long(11862098));
				setGenerate(isGenerate());
				setIssource(isNotSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(60915799));
				setCreatedts(new Timestamp(1380205277286L));
				setModifiedts(new Timestamp(1380205302867L));
				setOwnerpkstring(getTypeManagerManagedTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("owner");
				setQualifierinternal("owner");
				setAttributetypepk(getItemTypeCodePK());
				setIshidden(isAttributeShown());
				setColumnname("OwnerPkString");
				setEnclosingtypepk(getTypeManagerManagedTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("ownerPkString");
				setSuperattributedescriptorpk(new Long(51445847));
				setModifiers(new Integer(3101));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(86966359));
				setCreatedts(new Timestamp(1380205277819L));
				setModifiedts(new Timestamp(1380205303694L));
				setOwnerpkstring(getTypeManagerManagedTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("deprecated");
				setQualifierinternal("deprecated");
				setAttributetypepk(new Long(131153));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getTypeManagerManagedTypeCodePK());
				setPersistencetypepk(new Long(131153));
				setInheritancepathstring(",8796093186130,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(8465));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(26640471));
				setCreatedts(new Timestamp(1380205276480L));
				setModifiedts(new Timestamp(1380205302676L));
				setOwnerpkstring(getTypeManagerManagedTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("modifiedtime");
				setQualifierinternal("modifiedtime");
				setAttributetypepk(new Long(393297));
				setIshidden(isAttributeShown());
				setColumnname("modifiedTS");
				setEnclosingtypepk(getTypeManagerManagedTypeCodePK());
				setPersistencetypepk(new Long(393297));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("modifiedTimestampInternal");
				setSuperattributedescriptorpk(new Long(17170519));
				setModifiers(new Integer(1045));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(43778135));
				setCreatedts(new Timestamp(1380205276926L));
				setModifiedts(new Timestamp(1380205302504L));
				setOwnerpkstring(getTypeManagerManagedTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("itemtype");
				setQualifierinternal("itemtype");
				setAttributetypepk(getComposedTypeTypeCodePK());
				setIshidden(isAttributeShown());
				setColumnname("TypePkString");
				setEnclosingtypepk(getTypeManagerManagedTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("typePkString");
				setSuperattributedescriptorpk(new Long(34308183));
				setModifiers(new Integer(1055));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(356843607));
				setCreatedts(new Timestamp(1380205282065L));
				setModifiedts(new Timestamp(1380205301938L));
				setOwnerpkstring(getTypeManagerManagedTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("assignedcockpititemtemplates");
				setQualifierinternal("assignedCockpitItemTemplates");
				setAttributetypepk(new Long(7340115));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getTypeManagerManagedTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(347373655));
				setModifiers(new Integer(1055));
				setRelationname("Item2CockpitItemTemplateRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("cockpit");
				setOrdered(isOrdered());
				setRelationtype(new Long(13795410));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(9502807));
				setCreatedts(new Timestamp(1380205275922L));
				setModifiedts(new Timestamp(1380205302315L));
				setOwnerpkstring(getTypeManagerManagedTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("creationtime");
				setQualifierinternal("creationtime");
				setAttributetypepk(new Long(393297));
				setIshidden(isAttributeShown());
				setColumnname("createdTS");
				setEnclosingtypepk(getTypeManagerManagedTypeCodePK());
				setPersistencetypepk(new Long(393297));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("creationTimestampInternal");
				setSuperattributedescriptorpk(new Long(32855));
				setModifiers(new Integer(3101));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(78053463));
				setCreatedts(new Timestamp(1380205277640L));
				setModifiedts(new Timestamp(1380205303024L));
				setOwnerpkstring(getTypeManagerManagedTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("pk");
				setQualifierinternal("pk");
				setAttributetypepk(new Long(622673));
				setIshidden(isAttributeShown());
				setColumnname("PK");
				setEnclosingtypepk(getTypeManagerManagedTypeCodePK());
				setPersistencetypepk(new Long(622673));
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("pkString");
				setSuperattributedescriptorpk(new Long(68583511));
				setModifiers(new Integer(1045));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(87588951));
				setCreatedts(new Timestamp(1380205277828L));
				setModifiedts(new Timestamp(1380205303688L));
				setOwnerpkstring(getTypeManagerManagedTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("autocreate");
				setQualifierinternal("autocreate");
				setAttributetypepk(new Long(131153));
				setIshidden(isAttributeShown());
				setColumnname("p_autocreate");
				setEnclosingtypepk(getTypeManagerManagedTypeCodePK());
				setPersistencetypepk(new Long(131153));
				setInheritancepathstring(",8796093186130,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(275));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
				setDefaultvaluedefinitionstring("java.lang.Boolean.TRUE");
				setDefaultvalue(new byte[]
						{ -84, -19, 0, 5, 115, 114, 0, 17, 106, 97, 118, 97, 46, 108, 97, 110, 103, 46, 66, 111, 111, 108, 101, 97,
								110, -51,
								32, 114, -128, -43, -100, -6, -18, 2, 0, 1, 90, 0, 5, 118, 97, 108, 117, 101, 120, 112, 1 });
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(168984663));
				setCreatedts(new Timestamp(1380205279269L));
				setModifiedts(new Timestamp(1380205303182L));
				setOwnerpkstring(getTypeManagerManagedTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("savedvalues");
				setQualifierinternal("savedValues");
				setAttributetypepk(new Long(3833939));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getTypeManagerManagedTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(159514711));
				setModifiers(new Integer(1037));
				setRelationname("ItemSavedValuesRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setOrdered(isOrdered());
				setRelationtype(new Long(2818130));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(85721175));
				setCreatedts(new Timestamp(1380205277799L));
				setModifiedts(new Timestamp(1380205303713L));
				setOwnerpkstring(getTypeManagerManagedTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("name");
				setQualifierinternal("name");
				setAttributetypepk(new Long(65620));
				setIshidden(isAttributeShown());
				setColumnname("p_name");
				setEnclosingtypepk(getTypeManagerManagedTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796093186130,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(799));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(265683031));
				setCreatedts(new Timestamp(1380205280721L));
				setModifiedts(new Timestamp(1380205303480L));
				setOwnerpkstring(getTypeManagerManagedTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("synchronizedcopies");
				setQualifierinternal("synchronizedCopies");
				setAttributetypepk(new Long(2326611));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getTypeManagerManagedTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(256213079));
				setModifiers(new Integer(9229));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(86343767));
				setCreatedts(new Timestamp(1380205277810L));
				setModifiedts(new Timestamp(1380205303700L));
				setOwnerpkstring(getTypeManagerManagedTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("extensionname");
				setQualifierinternal("extensionName");
				setAttributetypepk(new Long(327761));
				setIshidden(isAttributeShown());
				setColumnname("p_extensionname");
				setEnclosingtypepk(getTypeManagerManagedTypeCodePK());
				setPersistencetypepk(new Long(327761));
				setInheritancepathstring(",8796093186130,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(283));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(655442));
				setPk(new Long(188678231));
				setCreatedts(new Timestamp(1380205279622L));
				setModifiedts(new Timestamp(1380205301739L));
				setOwnerpkstring(getTypeManagerManagedTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("alldocuments");
				setQualifierinternal("allDocuments");
				setAttributetypepk(new Long(4227155));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getTypeManagerManagedTypeCodePK());
				setInheritancepathstring(",8796093055058,8796093087826,8796093120594,8796093186130,");
				setIsproperty(isAttributeNotProperty());
				setSuperattributedescriptorpk(new Long(179208279));
				setModifiers(new Integer(1071));
				setRelationname("ItemDocrRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("commons");
				setOrdered(isNotOrdered());
				setRelationtype(new Long(4128850));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(3));
				setTypepkstring(new Long(393298));
				setPk(new Long(88211543));
				setCreatedts(new Timestamp(1380205277838L));
				setModifiedts(new Timestamp(1380205303706L));
				setOwnerpkstring(getTypeManagerManagedTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("generate");
				setQualifierinternal("generate");
				setAttributetypepk(new Long(131153));
				setIshidden(isAttributeShown());
				setColumnname("p_generate");
				setEnclosingtypepk(getTypeManagerManagedTypeCodePK());
				setPersistencetypepk(new Long(131153));
				setInheritancepathstring(",8796093186130,");
				setIsproperty(isAttributeProperty());
				setModifiers(new Integer(275));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(34308183));
				setCreatedts(new Timestamp(1380205276670L));
				setModifiedts(new Timestamp(1380205302453L));
				setOwnerpkstring(getItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("itemtype");
				setQualifierinternal("itemtype");
				setAttributetypepk(getComposedTypeTypeCodePK());
				setIshidden(isAttributeShown());
				setColumnname("TypePkString");
				setEnclosingtypepk(getItemTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796093055058,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("typePkString");
				setModifiers(new Integer(31));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(32855));
				setCreatedts(new Timestamp(1380205275490L));
				setModifiedts(new Timestamp(1380205302282L));
				setOwnerpkstring(getItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("creationtime");
				setQualifierinternal("creationtime");
				setAttributetypepk(new Long(393297));
				setIshidden(isAttributeShown());
				setColumnname("createdTS");
				setEnclosingtypepk(getItemTypeCodePK());
				setPersistencetypepk(new Long(393297));
				setInheritancepathstring(",8796093055058,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("creationTimestampInternal");
				setModifiers(new Integer(2077));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(68583511));
				setCreatedts(new Timestamp(1380205277451L));
				setModifiedts(new Timestamp(1380205302994L));
				setOwnerpkstring(getItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("pk");
				setQualifierinternal("pk");
				setAttributetypepk(new Long(622673));
				setIshidden(isAttributeShown());
				setColumnname("PK");
				setEnclosingtypepk(getItemTypeCodePK());
				setPersistencetypepk(new Long(622673));
				setInheritancepathstring(",8796093055058,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("pkString");
				setModifiers(new Integer(21));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(4));
				setTypepkstring(new Long(655442));
				setPk(new Long(347373655));
				setCreatedts(new Timestamp(1380205281958L));
				setModifiedts(new Timestamp(1380205301899L));
				setOwnerpkstring(getItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("assignedcockpititemtemplates");
				setQualifierinternal("assignedCockpitItemTemplates");
				setAttributetypepk(new Long(7340115));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getItemTypeCodePK());
				setInheritancepathstring(",8796093055058,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(31));
				setRelationname("Item2CockpitItemTemplateRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("cockpit");
				setOrdered(isOrdered());
				setRelationtype(new Long(13795410));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(4));
				setTypepkstring(new Long(655442));
				setPk(new Long(179208279));
				setCreatedts(new Timestamp(1380205279480L));
				setModifiedts(new Timestamp(1380205301700L));
				setOwnerpkstring(getItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("alldocuments");
				setQualifierinternal("allDocuments");
				setAttributetypepk(new Long(4227155));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getItemTypeCodePK());
				setInheritancepathstring(",8796093055058,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(47));
				setRelationname("ItemDocrRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("commons");
				setOrdered(isNotOrdered());
				setRelationtype(new Long(4128850));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(256213079));
				setCreatedts(new Timestamp(1380205280594L));
				setModifiedts(new Timestamp(1380205303449L));
				setOwnerpkstring(getItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("synchronizedcopies");
				setQualifierinternal("synchronizedCopies");
				setAttributetypepk(new Long(2326611));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getItemTypeCodePK());
				setInheritancepathstring(",8796093055058,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(8205));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(51445847));
				setCreatedts(new Timestamp(1380205277089L));
				setModifiedts(new Timestamp(1380205302835L));
				setOwnerpkstring(getItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("owner");
				setQualifierinternal("owner");
				setAttributetypepk(getItemTypeCodePK());
				setIshidden(isAttributeShown());
				setColumnname("OwnerPkString");
				setEnclosingtypepk(getItemTypeCodePK());
				setPersistencetypepk(new Long(491601));
				setInheritancepathstring(",8796093055058,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("ownerPkString");
				setModifiers(new Integer(2077));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(17170519));
				setCreatedts(new Timestamp(1380205276192L));
				setModifiedts(new Timestamp(1380205302643L));
				setOwnerpkstring(getItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("modifiedtime");
				setQualifierinternal("modifiedtime");
				setAttributetypepk(new Long(393297));
				setIshidden(isAttributeShown());
				setColumnname("modifiedTS");
				setEnclosingtypepk(getItemTypeCodePK());
				setPersistencetypepk(new Long(393297));
				setInheritancepathstring(",8796093055058,");
				setIsproperty(isAttributeNotProperty());
				setPersistencequalifierinternal("modifiedTimestampInternal");
				setModifiers(new Integer(21));
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setGenerate(isGenerate());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(4));
				setTypepkstring(new Long(655442));
				setPk(new Long(322568279));
				setCreatedts(new Timestamp(1380205281624L));
				setModifiedts(new Timestamp(1380205302095L));
				setOwnerpkstring(getItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("comments");
				setQualifierinternal("comments");
				setAttributetypepk(new Long(6455379));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getItemTypeCodePK());
				setInheritancepathstring(",8796093055058,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(31));
				setRelationname("CommentItemRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("comments");
				setOrdered(isOrdered());
				setRelationtype(new Long(11862098));
				setGenerate(isGenerate());
				setIssource(isNotSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(4));
				setTypepkstring(new Long(655442));
				setPk(new Long(159514711));
				setCreatedts(new Timestamp(1380205279127L));
				setModifiedts(new Timestamp(1380205303151L));
				setOwnerpkstring(getItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("savedvalues");
				setQualifierinternal("savedValues");
				setAttributetypepk(new Long(3833939));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getItemTypeCodePK());
				setInheritancepathstring(",8796093055058,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(13));
				setRelationname("ItemSavedValuesRelation");
				setAutocreate(isAutoCreate());
				setExtensionname("core");
				setOrdered(isOrdered());
				setRelationtype(new Long(2818130));
				setGenerate(isGenerate());
				setIssource(isSource());
			}
		});
		attributes.add(new AttributeRow()
		{
			{
				setHjmpts(new Long(2));
				setTypepkstring(new Long(393298));
				setPk(new Long(273350743));
				setCreatedts(new Timestamp(1380205280847L));
				setModifiedts(new Timestamp(1380205303303L));
				setOwnerpkstring(getItemTypeCodePK());
				setAclts(new Long(0));
				setPropts(new Long(0));
				setQualifierlowercaseinternal("synchronizationsources");
				setQualifierinternal("synchronizationSources");
				setAttributetypepk(new Long(2326611));
				setIshidden(isAttributeShown());
				setEnclosingtypepk(getItemTypeCodePK());
				setInheritancepathstring(",8796093055058,");
				setIsproperty(isAttributeNotProperty());
				setModifiers(new Integer(8205));
				setAutocreate(isAutoCreate());
				setExtensionname("catalog");
				setGenerate(isGenerate());
			}
		});
	}


	Boolean isOrdered()
	{
		return Boolean.TRUE;
	}

	Boolean isNotOrdered()
	{
		return Boolean.FALSE;
	}

	Boolean isUnique()
	{
		return Boolean.TRUE;
	}

	Boolean isNotSource()
	{
		return Boolean.FALSE;
	}

	Boolean isSource()
	{
		return Boolean.TRUE;
	}

	Boolean isGenerate()
	{
		return Boolean.TRUE;
	}

	Boolean isAutoCreate()
	{
		return Boolean.TRUE;
	}

	Boolean isAttributeHidden()
	{
		return Boolean.TRUE;
	}

	Boolean isAttributeShown()
	{
		return Boolean.FALSE;
	}

	Boolean isAttributeNotProperty()
	{
		return Boolean.FALSE;
	}

	Boolean isAttributeProperty()
	{
		return Boolean.TRUE;
	}
}
