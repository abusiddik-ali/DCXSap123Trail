/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.ordercloning.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyMap;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.same;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.type.ComposedTypeModel;
import de.hybris.platform.order.AbstractOrderEntryTypeService;
import de.hybris.platform.order.strategies.ordercloning.AdjustEntryTestHook;
import de.hybris.platform.order.strategies.ordercloning.CloneAbstractOrderHook;
import de.hybris.platform.order.strategies.ordercloning.impl.DefaultCloneAbstractOrderStrategy.OrderCopyContext;
import de.hybris.platform.servicelayer.internal.model.impl.ItemModelCloneCreator;
import de.hybris.platform.servicelayer.internal.model.impl.ItemModelCloneCreator.CopyContext;
import de.hybris.platform.servicelayer.type.TypeService;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.spockframework.util.CollectionUtil;


@SuppressWarnings("javadoc")
@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class DefaultCloneAbstractOrderStrategyTest
{
	private static final String ATTRIBUTE_TO_SKIP = "attributeToSkip";

	private DefaultCloneAbstractOrderStrategy classUnderTest;

	@Mock
	private TypeService typeService;
	@Mock
	private ItemModelCloneCreator itemModelCloneCreator;
	@Mock
	private AbstractOrderEntryTypeService abstractOrderEntryTypeService;
	@Mock
	private Set<String> skippedAttributes;
	@Mock
	private ComposedTypeModel composedType;
	@Mock
	private AbstractOrderModel sourceOrder;
	@Mock
	private AbstractOrderEntryModel sourceOrderEntry;
	@Mock
	private AbstractOrderModel targetOrder;
	@Mock
	private AbstractOrderEntryModel targetOrderEntry;
	@Mock
	private CloneAbstractOrderHook hook1;
	@Mock
	private CloneAbstractOrderHook hook2;

	private final List<String> skippedAttributesList = new ArrayList<>(List.of(ATTRIBUTE_TO_SKIP));
	private final List<CloneAbstractOrderHook> cloneHooks = new ArrayList<>();
	private final CloneAbstractOrderHook hook3 = new AdjustEntryTestHook();

	@Before
	public void setup()
	{
		// inject mocks does not work here, as we have two lists that get not passed to constructor in correct order
		// so we call the constructor manually
		classUnderTest = new DefaultCloneAbstractOrderStrategy(typeService, itemModelCloneCreator, abstractOrderEntryTypeService,
				skippedAttributesList);
		classUnderTest.setCloneHooks(cloneHooks);
		when(sourceOrder.getEntries()).thenReturn(CollectionUtil.listOf(sourceOrderEntry));
		when(targetOrder.getEntries()).thenReturn(CollectionUtil.listOf(targetOrderEntry));
		when(skippedAttributes.contains(ATTRIBUTE_TO_SKIP)).thenReturn(true);
		when(itemModelCloneCreator.copy(any(ComposedTypeModel.class), eq(sourceOrder), any(CopyContext.class)))
				.thenReturn(targetOrder);

		cloneHooks.add(hook1);
		cloneHooks.add(hook2);
		cloneHooks.add(hook3);
	}

	@Test
	public void testOrderCopyContext()
	{
		final OrderCopyContext copyContextUnderTest = new OrderCopyContext(composedType, skippedAttributes);
		assertNotNull(copyContextUnderTest);
		assertTrue(copyContextUnderTest.skipAttribute(null, ATTRIBUTE_TO_SKIP));
		assertFalse(copyContextUnderTest.skipAttribute(null, "some attribute to be copied"));
		assertEquals(composedType, copyContextUnderTest.getTargetType(sourceOrderEntry));
		assertNull(copyContextUnderTest.getTargetType(sourceOrder));
	}

	@Test
	public void testCloneEntriesCopyContext()
	{
		final OrderCopyContext copyContextUnderTest = classUnderTest.createCloneEntriesCopyContext();
		assertTrue(copyContextUnderTest.skipAttribute(null, AbstractOrderEntryModel.ORDER));
		assertTrue(copyContextUnderTest.skipAttribute(null, ATTRIBUTE_TO_SKIP));
		assertFalse(copyContextUnderTest.skipAttribute(null, "some attribute to be copied"));
	}

	@Test
	public void testClone()
	{
		final AbstractOrderModel result = classUnderTest.clone(composedType, composedType, sourceOrder, "code", CartModel.class,
				CartEntryModel.class);
		assertNotNull(result);
	}

	@Test
	public void testCloneNoTypeGiven()
	{
		final AbstractOrderModel result = classUnderTest.clone(null, null, sourceOrder, null, CartModel.class,
				CartEntryModel.class);
		assertNotNull(result);
	}

	@Test
	public void testCloneAssignableTargetTypeGiven()
	{
		final AbstractOrderModel result = classUnderTest.clone(null, null, sourceOrder, null, AbstractOrderModel.class,
				AbstractOrderEntryModel.class);
		assertNotNull(result);
	}

	@Test(expected = IllegalStateException.class)
	public void testCloneWrongTargetEntrySize()
	{
		when(targetOrder.getEntries()).thenReturn(List.of());
		classUnderTest.clone(null, null, sourceOrder, null, CartModel.class, CartEntryModel.class);
	}

	@Test
	public void testCloneNullTargetEntries()
	{
		when(sourceOrder.getEntries()).thenReturn(List.of());
		when(targetOrder.getEntries()).thenReturn(null);
		final AbstractOrderModel result = classUnderTest.clone(null, null, sourceOrder, null, AbstractOrderModel.class,
				AbstractOrderEntryModel.class);
		assertNotNull(result);
	}

	@Test
	public void testCloneCallsHooks()
	{
		classUnderTest.clone(composedType, composedType, sourceOrder, "code", CartModel.class, CartEntryModel.class);
		verify(hook1).beforeClone(sourceOrder, CartModel.class);
		verify(hook2).beforeClone(sourceOrder, CartModel.class);
		verify(hook1).afterClone(sourceOrder, targetOrder, CartModel.class);
		verify(hook2).afterClone(sourceOrder, targetOrder, CartModel.class);
		verify(hook1).adjustEntryNumbers(anyMap());
		verify(hook2).adjustEntryNumbers(anyMap());
	}

	@Test
	public void testCloneEntriesCallsHooks()
	{
		when(itemModelCloneCreator.copyAll(any(), any())).thenReturn(new ArrayList<>());
		classUnderTest.cloneEntries(composedType, sourceOrder);
		verify(hook1).beforeCloneEntries(sourceOrder);
		verify(hook2).beforeCloneEntries(sourceOrder);
		verify(hook1).afterCloneEntries(same(sourceOrder), any(List.class));
		verify(hook2).afterCloneEntries(same(sourceOrder), any(List.class));

	}

	@Test
	public void testCloneEntriesEmptyCallsHooks()
	{
		classUnderTest.cloneEntries(composedType, sourceOrder);
		verify(hook1).beforeCloneEntries(sourceOrder);
		verify(hook2).beforeCloneEntries(sourceOrder);
		verify(hook1).afterCloneEntries(sourceOrder, Collections.emptyList());
		verify(hook2).afterCloneEntries(sourceOrder, Collections.emptyList());
	}

	@Test
	public void testBeforeClone()
	{
		classUnderTest.beforeClone(sourceOrder, AbstractOrderModel.class);
		verify(hook1).beforeClone(sourceOrder, AbstractOrderModel.class);
		verify(hook2).beforeClone(sourceOrder, AbstractOrderModel.class);
		verifyNoMoreInteractions(hook1, hook2);
	}

	@Test
	public void testAfterClone()
	{
		classUnderTest.afterClone(sourceOrder, targetOrder, AbstractOrderModel.class);
		verify(hook1).afterClone(sourceOrder, targetOrder, AbstractOrderModel.class);
		verify(hook2).afterClone(sourceOrder, targetOrder, AbstractOrderModel.class);
		verifyNoMoreInteractions(hook1, hook2);
	}

	@Test
	public void testBeforeCloneEntries()
	{
		classUnderTest.beforeCloneEntries(sourceOrder);
		verify(hook1).beforeCloneEntries(sourceOrder);
		verify(hook2).beforeCloneEntries(sourceOrder);
		verifyNoMoreInteractions(hook1, hook2);
	}

	@Test
	public void testAfterCloneEntries()
	{
		classUnderTest.afterCloneEntries(sourceOrder, Collections.EMPTY_LIST);
		verify(hook1).afterCloneEntries(sourceOrder, Collections.EMPTY_LIST);
		verify(hook2).afterCloneEntries(sourceOrder, Collections.EMPTY_LIST);
		verifyNoMoreInteractions(hook1, hook2);
	}

	@Test
	public void testAdjustEntryNumbers()
	{
		final Map entryNumberMappings = new HashMap();
		classUnderTest.adjustEntryNumbers(entryNumberMappings);
		verify(hook1).adjustEntryNumbers(entryNumberMappings);
		verify(hook2).adjustEntryNumbers(entryNumberMappings);
		verifyNoMoreInteractions(hook1, hook2);
	}

	public void testBeforeCloneEmpty()
	{
		cloneHooks.clear();
		classUnderTest.beforeClone(sourceOrder, AbstractOrderModel.class);
		verifyZeroInteractions(hook1, hook2);
	}

	@Test
	public void testAfterCloneEmpty()
	{
		cloneHooks.clear();
		classUnderTest.afterClone(sourceOrder, targetOrder, AbstractOrderModel.class);
		verifyZeroInteractions(hook1, hook2);
	}

	@Test
	public void testBeforeCloneEntriesEmpty()
	{
		cloneHooks.clear();
		classUnderTest.beforeCloneEntries(sourceOrder);
		verifyZeroInteractions(hook1, hook2);
	}

	@Test
	public void testAfterCloneEntriesEmpty()
	{
		cloneHooks.clear();
		classUnderTest.afterCloneEntries(sourceOrder, Collections.EMPTY_LIST);
		verifyZeroInteractions(hook1, hook2);
	}

	@Test
	public void testCloneBackwardCompatibility()
	{
		classUnderTest = new DefaultCloneAbstractOrderStrategy(typeService, itemModelCloneCreator, abstractOrderEntryTypeService,
				skippedAttributesList);
		final AbstractOrderModel result = classUnderTest.clone(composedType, composedType, sourceOrder, "code", CartModel.class,
				CartEntryModel.class);
		assertNotNull(result);
		verifyZeroInteractions(hook1, hook2);
	}

}
