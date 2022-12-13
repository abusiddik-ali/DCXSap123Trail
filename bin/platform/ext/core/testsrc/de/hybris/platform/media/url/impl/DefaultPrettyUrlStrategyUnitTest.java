/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.media.url.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.media.url.PrettyUrlStrategy;

import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;

import org.apache.commons.lang3.tuple.Pair;
import org.junit.Test;

@UnitTest
public class DefaultPrettyUrlStrategyUnitTest
{
	private static final Pair<PrettyUrlStrategy.MediaData, Object>[] ASSEMBLING_TEST_CASES = new Pair[]{
			successfulAssembling("testtenant", "root", "8796716531742/test.21.txt", "test.21.txt",
					"sys_testtenant/root/8796716531742/test.21/test.21.txt"),
			successfulAssembling("testtenant", "root", "8796716531742/test.htm", "test.htm",
					"sys_testtenant/root/8796716531742/test/test.htm"),
			successfulAssembling("testtenant", "root", "8796716531742/test.html", "test.html",
					"sys_testtenant/root/8796716531742/test/test.html"),
			successfulAssembling("testtenant", "root", "8796716531742/test.jpeg", "test.jpeg",
					"sys_testtenant/root/8796716531742/test/test.jpeg"),
			successfulAssembling("testtenant", "root", "8796716531742/test.jpg", "test.jpg",
					"sys_testtenant/root/8796716531742/test/test.jpg"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.bin", null,
					"sys_testtenant/root/hfd/hcc/8796716531742/8796716531742.bin"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.bin", "test",
					"sys_testtenant/root/hfd/hcc/8796716531742/test.bin"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.bin", "test.21.txt",
					"sys_testtenant/root/hfd/hcc/8796716531742/test.21.bin"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.bin", "test.htm",
					"sys_testtenant/root/hfd/hcc/8796716531742/test.bin"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.bin", "test.html",
					"sys_testtenant/root/hfd/hcc/8796716531742/test.bin"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.bin", "test.jpeg",
					"sys_testtenant/root/hfd/hcc/8796716531742/test.bin"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.bin", "test.jpg",
					"sys_testtenant/root/hfd/hcc/8796716531742/test.bin"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.dat", null,
					"sys_testtenant/root/hfd/hcc/8796716531742/8796716531742.dat"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.dat", "test",
					"sys_testtenant/root/hfd/hcc/8796716531742/test.dat"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.dat", "test.21.txt",
					"sys_testtenant/root/hfd/hcc/8796716531742/test.21.dat"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.dat", "test.htm",
					"sys_testtenant/root/hfd/hcc/8796716531742/test.dat"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.dat", "test.html",
					"sys_testtenant/root/hfd/hcc/8796716531742/test.dat"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.dat", "test.jpeg",
					"sys_testtenant/root/hfd/hcc/8796716531742/test.dat"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.dat", "test.jpg",
					"sys_testtenant/root/hfd/hcc/8796716531742/test.dat"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.html", null,
					"sys_testtenant/root/hfd/hcc/8796716531742/8796716531742.html"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.html", "test",
					"sys_testtenant/root/hfd/hcc/8796716531742/test.html"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.html", "test.21.txt",
					"sys_testtenant/root/hfd/hcc/8796716531742/test.21.html"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.html", "test.htm",
					"sys_testtenant/root/hfd/hcc/8796716531742/test.html"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.html", "test.html",
					"sys_testtenant/root/hfd/hcc/8796716531742/test.html"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.html", "test.jpeg",
					"sys_testtenant/root/hfd/hcc/8796716531742/test.html"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.html", "test.jpg",
					"sys_testtenant/root/hfd/hcc/8796716531742/test.html"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.jpg", null,
					"sys_testtenant/root/hfd/hcc/8796716531742/8796716531742.jpg"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.jpg", "test",
					"sys_testtenant/root/hfd/hcc/8796716531742/test.jpg"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.jpg", "test.21.txt",
					"sys_testtenant/root/hfd/hcc/8796716531742/test.21.jpg"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.jpg", "test.htm",
					"sys_testtenant/root/hfd/hcc/8796716531742/test.jpg"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.jpg", "test.html",
					"sys_testtenant/root/hfd/hcc/8796716531742/test.jpg"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.jpg", "test.jpeg",
					"sys_testtenant/root/hfd/hcc/8796716531742/test.jpg"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742.jpg", "test.jpg",
					"sys_testtenant/root/hfd/hcc/8796716531742/test.jpg"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742/test.21.txt", "test.21.txt",
					"sys_testtenant/root/hfd/hcc/8796716531742/test.21/test.21.txt"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742/test.htm", "test.htm",
					"sys_testtenant/root/hfd/hcc/8796716531742/test/test.htm"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742/test.html", "test.html",
					"sys_testtenant/root/hfd/hcc/8796716531742/test/test.html"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742/test.jpeg", "test.jpeg",
					"sys_testtenant/root/hfd/hcc/8796716531742/test/test.jpeg"),
			successfulAssembling("testtenant", "root", "hfd/hcc/8796716531742/test.jpg", "test.jpg",
					"sys_testtenant/root/hfd/hcc/8796716531742/test/test.jpg"),
			successfulAssembling("testtenant", "root", "sys-testtenant/root/h8b/h48/8796486303774/test.21.txt", "test.21.txt",
					"sys_testtenant/root/sys-testtenant/root/h8b/h48/8796486303774/test.21/test.21.txt"),
			successfulAssembling("testtenant", "root", "sys-testtenant/root/h8b/h48/8796486303774/test.htm", "test.htm",
					"sys_testtenant/root/sys-testtenant/root/h8b/h48/8796486303774/test/test.htm"),
			successfulAssembling("testtenant", "root", "sys-testtenant/root/h8b/h48/8796486303774/test.html", "test.html",
					"sys_testtenant/root/sys-testtenant/root/h8b/h48/8796486303774/test/test.html"),
			successfulAssembling("testtenant", "root", "sys-testtenant/root/h8b/h48/8796486303774/test.jpeg", "test.jpeg",
					"sys_testtenant/root/sys-testtenant/root/h8b/h48/8796486303774/test/test.jpeg"),
			successfulAssembling("testtenant", "root", "sys-testtenant/root/h8b/h48/8796486303774/test.jpg", "test.jpg",
					"sys_testtenant/root/sys-testtenant/root/h8b/h48/8796486303774/test/test.jpg"),
			successfulAssembling("testtenant", "testfolder", "8796716531742/test.21.txt", "test.21.txt",
					"sys_testtenant/testfolder/8796716531742/test.21/test.21.txt"),
			successfulAssembling("testtenant", "testfolder", "8796716531742/test.htm", "test.htm",
					"sys_testtenant/testfolder/8796716531742/test/test.htm"),
			successfulAssembling("testtenant", "testfolder", "8796716531742/test.html", "test.html",
					"sys_testtenant/testfolder/8796716531742/test/test.html"),
			successfulAssembling("testtenant", "testfolder", "8796716531742/test.jpeg", "test.jpeg",
					"sys_testtenant/testfolder/8796716531742/test/test.jpeg"),
			successfulAssembling("testtenant", "testfolder", "8796716531742/test.jpg", "test.jpg",
					"sys_testtenant/testfolder/8796716531742/test/test.jpg"),
			successfulAssembling("testtenant", "testfolder", "hfd/hcc/8796716531742/test.21.txt", "test.21.txt",
					"sys_testtenant/testfolder/hfd/hcc/8796716531742/test.21/test.21.txt"),
			successfulAssembling("testtenant", "testfolder", "hfd/hcc/8796716531742/test.htm", "test.htm",
					"sys_testtenant/testfolder/hfd/hcc/8796716531742/test/test.htm"),
			successfulAssembling("testtenant", "testfolder", "hfd/hcc/8796716531742/test.html", "test.html",
					"sys_testtenant/testfolder/hfd/hcc/8796716531742/test/test.html"),
			successfulAssembling("testtenant", "testfolder", "hfd/hcc/8796716531742/test.jpeg", "test.jpeg",
					"sys_testtenant/testfolder/hfd/hcc/8796716531742/test/test.jpeg"),
			successfulAssembling("testtenant", "testfolder", "hfd/hcc/8796716531742/test.jpg", "test.jpg",
					"sys_testtenant/testfolder/hfd/hcc/8796716531742/test/test.jpg"),
			successfulAssembling("testtenant", "testfolder", "sys-testtenant/testfolder/h8b/h48/8796486303774/test.21.txt",
					"test.21.txt",
					"sys_testtenant/testfolder/sys-testtenant/testfolder/h8b/h48/8796486303774/test.21/test.21.txt"),
			successfulAssembling("testtenant", "testfolder", "sys-testtenant/testfolder/h8b/h48/8796486303774/test.htm",
					"test.htm", "sys_testtenant/testfolder/sys-testtenant/testfolder/h8b/h48/8796486303774/test/test.htm"),
			successfulAssembling("testtenant", "testfolder", "sys-testtenant/testfolder/h8b/h48/8796486303774/test.html",
					"test.html", "sys_testtenant/testfolder/sys-testtenant/testfolder/h8b/h48/8796486303774/test/test.html"),
			successfulAssembling("testtenant", "testfolder", "sys-testtenant/testfolder/h8b/h48/8796486303774/test.jpeg",
					"test.jpeg", "sys_testtenant/testfolder/sys-testtenant/testfolder/h8b/h48/8796486303774/test/test.jpeg"),
			successfulAssembling("testtenant", "testfolder", "sys-testtenant/testfolder/h8b/h48/8796486303774/test.jpg",
					"test.jpg", "sys_testtenant/testfolder/sys-testtenant/testfolder/h8b/h48/8796486303774/test/test.jpg"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.bin", null,
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/8796716531742.bin"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.bin", "test",
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.bin"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.bin", "test.21.txt",
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.21.bin"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.bin", "test.htm",
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.bin"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.bin", "test.html",
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.bin"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.bin", "test.jpeg",
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.bin"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.bin", "test.jpg",
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.bin"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.dat", null,
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/8796716531742.dat"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.dat", "test",
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.dat"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.dat", "test.21.txt",
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.21.dat"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.dat", "test.htm",
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.dat"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.dat", "test.html",
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.dat"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.dat", "test.jpeg",
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.dat"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.dat", "test.jpg",
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.dat"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.html", null,
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/8796716531742.html"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.html", "test",
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.html"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.html", "test.21.txt",
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.21.html"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.html", "test.htm",
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.html"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.html", "test.html",
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.html"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.html", "test.jpeg",
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.html"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.html", "test.jpg",
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.html"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.jpg", null,
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/8796716531742.jpg"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.jpg", "test",
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.jpg"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.jpg", "test.21.txt",
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.21.jpg"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.jpg", "test.htm",
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.jpg"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.jpg", "test.html",
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.jpg"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.jpg", "test.jpeg",
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.jpg"),
			successfulAssembling("testtenant", "testfolder", "testfolderpath/hfd/hcc/8796716531742.jpg", "test.jpg",
					"sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.jpg"),
			unsupportedAssembling("testtenant", "root", "8796716531742", null),
			unsupportedAssembling("testtenant", "root", "8796716531742/test", "test"),
			unsupportedAssembling("testtenant", "root", "hfd/hcc/8796716531742", null),
			unsupportedAssembling("testtenant", "root", "hfd/hcc/8796716531742/test", "test"),
			unsupportedAssembling("testtenant", "root", "sys-testtenant/root/h8b/h48/8796486303774", null),
			unsupportedAssembling("testtenant", "root", "sys-testtenant/root/h8b/h48/8796486303774/test", "test"),
			unsupportedAssembling("testtenant", "testfolder", "8796716531742", null),
			unsupportedAssembling("testtenant", "testfolder", "8796716531742/test", "test"),
			unsupportedAssembling("testtenant", "testfolder", "hfd/hcc/8796716531742", null),
			unsupportedAssembling("testtenant", "testfolder", "hfd/hcc/8796716531742/test", "test"),
			unsupportedAssembling("testtenant", "testfolder", "sys-testtenant/testfolder/h8b/h48/8796486303774", null),
			unsupportedAssembling("testtenant", "testfolder", "sys-testtenant/testfolder/h8b/h48/8796486303774/test", "test")
	};
	private static final Pair<String, Object>[] PARSING_TEST_CASES = new Pair[]{
			successfulParsing("sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.jpg", "testtenant",
					"testfolder", "testfolderpath/hfd/hcc/8796716531742.jpg"),
			successfulParsing("sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.html", "testtenant",
					"testfolder", "testfolderpath/hfd/hcc/8796716531742.html"),
			successfulParsing("sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.dat", "testtenant",
					"testfolder", "testfolderpath/hfd/hcc/8796716531742.dat"),
			successfulParsing("sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.bin", "testtenant",
					"testfolder", "testfolderpath/hfd/hcc/8796716531742.bin"),
			successfulParsing("sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.21.jpg", "testtenant",
					"testfolder", "testfolderpath/hfd/hcc/8796716531742.jpg"),
			successfulParsing("sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.21.html", "testtenant",
					"testfolder", "testfolderpath/hfd/hcc/8796716531742.html"),
			successfulParsing("sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.21.dat", "testtenant",
					"testfolder", "testfolderpath/hfd/hcc/8796716531742.dat"),
			successfulParsing("sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/test.21.bin", "testtenant",
					"testfolder", "testfolderpath/hfd/hcc/8796716531742.bin"),
			successfulParsing("sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/8796716531742.jpg", "testtenant",
					"testfolder", "testfolderpath/hfd/hcc/8796716531742.jpg"),
			successfulParsing("sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/8796716531742.html", "testtenant",
					"testfolder", "testfolderpath/hfd/hcc/8796716531742.html"),
			successfulParsing("sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/8796716531742.dat", "testtenant",
					"testfolder", "testfolderpath/hfd/hcc/8796716531742.dat"),
			successfulParsing("sys_testtenant/testfolder/testfolderpath/hfd/hcc/8796716531742/8796716531742.bin", "testtenant",
					"testfolder", "testfolderpath/hfd/hcc/8796716531742.bin"),
			successfulParsing("sys_testtenant/testfolder/sys-testtenant/testfolder/h8b/h48/8796486303774/test/test.jpg",
					"testtenant", "testfolder", "sys-testtenant/testfolder/h8b/h48/8796486303774/test.jpg"),
			successfulParsing("sys_testtenant/testfolder/sys-testtenant/testfolder/h8b/h48/8796486303774/test/test.jpeg",
					"testtenant", "testfolder", "sys-testtenant/testfolder/h8b/h48/8796486303774/test.jpeg"),
			successfulParsing("sys_testtenant/testfolder/sys-testtenant/testfolder/h8b/h48/8796486303774/test/test.html",
					"testtenant", "testfolder", "sys-testtenant/testfolder/h8b/h48/8796486303774/test.html"),
			successfulParsing("sys_testtenant/testfolder/sys-testtenant/testfolder/h8b/h48/8796486303774/test/test.htm",
					"testtenant", "testfolder", "sys-testtenant/testfolder/h8b/h48/8796486303774/test.htm"),
			successfulParsing("sys_testtenant/testfolder/sys-testtenant/testfolder/h8b/h48/8796486303774/test.21/test.21.txt",
					"testtenant", "testfolder", "sys-testtenant/testfolder/h8b/h48/8796486303774/test.21.txt"),
			successfulParsing("sys_testtenant/testfolder/hfd/hcc/8796716531742/test/test.jpg", "testtenant", "testfolder",
					"hfd/hcc/8796716531742/test.jpg"),
			successfulParsing("sys_testtenant/testfolder/hfd/hcc/8796716531742/test/test.jpeg", "testtenant", "testfolder",
					"hfd/hcc/8796716531742/test.jpeg"),
			successfulParsing("sys_testtenant/testfolder/hfd/hcc/8796716531742/test/test.html", "testtenant", "testfolder",
					"hfd/hcc/8796716531742/test.html"),
			successfulParsing("sys_testtenant/testfolder/hfd/hcc/8796716531742/test/test.htm", "testtenant", "testfolder",
					"hfd/hcc/8796716531742/test.htm"),
			successfulParsing("sys_testtenant/testfolder/hfd/hcc/8796716531742/test.21/test.21.txt", "testtenant", "testfolder",
					"hfd/hcc/8796716531742/test.21.txt"),
			successfulParsing("sys_testtenant/testfolder/8796716531742/test/test.jpg", "testtenant", "testfolder",
					"8796716531742/test.jpg"),
			successfulParsing("sys_testtenant/testfolder/8796716531742/test/test.jpeg", "testtenant", "testfolder",
					"8796716531742/test.jpeg"),
			successfulParsing("sys_testtenant/testfolder/8796716531742/test/test.html", "testtenant", "testfolder",
					"8796716531742/test.html"),
			successfulParsing("sys_testtenant/testfolder/8796716531742/test/test.htm", "testtenant", "testfolder",
					"8796716531742/test.htm"),
			successfulParsing("sys_testtenant/testfolder/8796716531742/test.21/test.21.txt", "testtenant", "testfolder",
					"8796716531742/test.21.txt"),
			successfulParsing("sys_testtenant/root/sys-testtenant/root/h8b/h48/8796486303774/test/test.jpg", "testtenant", "root",
					"sys-testtenant/root/h8b/h48/8796486303774/test.jpg"),
			successfulParsing("sys_testtenant/root/sys-testtenant/root/h8b/h48/8796486303774/test/test.jpeg", "testtenant",
					"root", "sys-testtenant/root/h8b/h48/8796486303774/test.jpeg"),
			successfulParsing("sys_testtenant/root/sys-testtenant/root/h8b/h48/8796486303774/test/test.html", "testtenant",
					"root", "sys-testtenant/root/h8b/h48/8796486303774/test.html"),
			successfulParsing("sys_testtenant/root/sys-testtenant/root/h8b/h48/8796486303774/test/test.htm", "testtenant", "root",
					"sys-testtenant/root/h8b/h48/8796486303774/test.htm"),
			successfulParsing("sys_testtenant/root/sys-testtenant/root/h8b/h48/8796486303774/test.21/test.21.txt", "testtenant",
					"root", "sys-testtenant/root/h8b/h48/8796486303774/test.21.txt"),
			successfulParsing("sys_testtenant/root/hfd/hcc/8796716531742/test/test.jpg", "testtenant", "root",
					"hfd/hcc/8796716531742/test.jpg"),
			successfulParsing("sys_testtenant/root/hfd/hcc/8796716531742/test/test.jpeg", "testtenant", "root",
					"hfd/hcc/8796716531742/test.jpeg"),
			successfulParsing("sys_testtenant/root/hfd/hcc/8796716531742/test/test.html", "testtenant", "root",
					"hfd/hcc/8796716531742/test.html"),
			successfulParsing("sys_testtenant/root/hfd/hcc/8796716531742/test/test.htm", "testtenant", "root",
					"hfd/hcc/8796716531742/test.htm"),
			successfulParsing("sys_testtenant/root/hfd/hcc/8796716531742/test.jpg", "testtenant", "root",
					"hfd/hcc/8796716531742.jpg"),
			successfulParsing("sys_testtenant/root/hfd/hcc/8796716531742/test.html", "testtenant", "root",
					"hfd/hcc/8796716531742.html"),
			successfulParsing("sys_testtenant/root/hfd/hcc/8796716531742/test.dat", "testtenant", "root",
					"hfd/hcc/8796716531742.dat"),
			successfulParsing("sys_testtenant/root/hfd/hcc/8796716531742/test.bin", "testtenant", "root",
					"hfd/hcc/8796716531742.bin"),
			successfulParsing("sys_testtenant/root/hfd/hcc/8796716531742/test.21/test.21.txt", "testtenant", "root",
					"hfd/hcc/8796716531742/test.21.txt"),
			successfulParsing("sys_testtenant/root/hfd/hcc/8796716531742/test.21.jpg", "testtenant", "root",
					"hfd/hcc/8796716531742.jpg"),
			successfulParsing("sys_testtenant/root/hfd/hcc/8796716531742/test.21.html", "testtenant", "root",
					"hfd/hcc/8796716531742.html"),
			successfulParsing("sys_testtenant/root/hfd/hcc/8796716531742/test.21.dat", "testtenant", "root",
					"hfd/hcc/8796716531742.dat"),
			successfulParsing("sys_testtenant/root/hfd/hcc/8796716531742/test.21.bin", "testtenant", "root",
					"hfd/hcc/8796716531742.bin"),
			successfulParsing("sys_testtenant/root/hfd/hcc/8796716531742/8796716531742.jpg", "testtenant", "root",
					"hfd/hcc/8796716531742.jpg"),
			successfulParsing("sys_testtenant/root/hfd/hcc/8796716531742/8796716531742.html", "testtenant", "root",
					"hfd/hcc/8796716531742.html"),
			successfulParsing("sys_testtenant/root/hfd/hcc/8796716531742/8796716531742.dat", "testtenant", "root",
					"hfd/hcc/8796716531742.dat"),
			successfulParsing("sys_testtenant/root/hfd/hcc/8796716531742/8796716531742.bin", "testtenant", "root",
					"hfd/hcc/8796716531742.bin"),
			successfulParsing("sys_testtenant/root/8796716531742/test/test.jpg", "testtenant", "root", "8796716531742/test.jpg"),
			successfulParsing("sys_testtenant/root/8796716531742/test/test.jpeg", "testtenant", "root",
					"8796716531742/test.jpeg"),
			successfulParsing("sys_testtenant/root/8796716531742/test/test.html", "testtenant", "root",
					"8796716531742/test.html"),
			successfulParsing("sys_testtenant/root/8796716531742/test/test.htm", "testtenant", "root", "8796716531742/test.htm"),
			successfulParsing("sys_testtenant/root/8796716531742/test.21/test.21.txt", "testtenant", "root",
					"8796716531742/test.21.txt"),
			successfulParsing("sys_strange1/strange2/strange3/strange4/", "strange1", "strange2",
					"strange3/strange4.strange3/strange4/"),
			successfulParsing("sys_strange1/strange2/strange3/strange4", "strange1", "strange2", "strange3.strange3/strange4"),
			successfulParsing("sys_strange1/strange2/strange3/", "strange1", "strange2", "strange3.strange3/"),
			failedParsing("wrong/wrong/wrong/wrong/wrong.txt", IllegalArgumentException.class),
			failedParsing("wrong/wrong/wrong/wrong/", IllegalArgumentException.class),
			failedParsing("wrong/wrong/wrong/wrong", IllegalArgumentException.class),
			failedParsing("wrong/wrong/wrong/", IllegalArgumentException.class),
			failedParsing("wrong/wrong/wrong", IllegalArgumentException.class),
			failedParsing("wrong/wrong/", IllegalArgumentException.class),
			failedParsing("wrong/wrong", IllegalArgumentException.class),
			failedParsing("wrong/", IllegalArgumentException.class),
			failedParsing("wrong", IllegalArgumentException.class),
			failedParsing("sys_wrong/wrong/wrong", IllegalArgumentException.class),
			failedParsing("sys_wrong/wrong/", IllegalArgumentException.class),
			failedParsing("sys_wrong/wrong", IllegalArgumentException.class),
			failedParsing("sys_wrong/", IllegalArgumentException.class),
			failedParsing("sys_wrong", IllegalArgumentException.class),
			failedParsing("sys_", IllegalArgumentException.class),
			failedParsing("", IllegalArgumentException.class)
	};

	private static Pair<PrettyUrlStrategy.MediaData, Object> successfulAssembling(final String givenTenant,
	                                                                              final String givenFolder,
	                                                                              final String givenLocation,
	                                                                              final String givenFileName,
	                                                                              final String expectedPath)
	{
		return assemblingTestCase(givenTenant, givenFolder, givenLocation, givenFileName, Optional.of(expectedPath));
	}

	private static Pair<PrettyUrlStrategy.MediaData, Object> unsupportedAssembling(final String givenTenant,
	                                                                               final String givenFolder,
	                                                                               final String givenLocation,
	                                                                               final String givenFileName)
	{
		return assemblingTestCase(givenTenant, givenFolder, givenLocation, givenFileName, Optional.empty());
	}

	private static Pair<PrettyUrlStrategy.MediaData, Object> assemblingTestCase(final String givenTenant,
	                                                                            final String givenFolder,
	                                                                            final String givenLocation,
	                                                                            final String givenFileName,
	                                                                            final Object expectedResult)
	{
		final PrettyUrlStrategy.MediaData mediaData = new PrettyUrlStrategy.MediaData(givenTenant, givenFolder, givenLocation);
		mediaData.setRealFileName(givenFileName);
		return Pair.of(mediaData, Objects.requireNonNull(expectedResult));
	}

	private static Pair<String, Object> successfulParsing(final String givenPath, final String expectedTenant,
	                                                      final String expectedFolder, final String expectedLocation)
	{
		final PrettyUrlStrategy.ParsedPath expectedPath = new PrettyUrlStrategy.ParsedPath(expectedTenant, expectedFolder,
				expectedLocation);
		return parsingTestCase(givenPath, expectedPath);
	}

	private static Pair<String, Object> failedParsing(final String givenPath,
	                                                  final Class<? extends Exception> expectedExceptionClass)
	{
		return parsingTestCase(givenPath, expectedExceptionClass);
	}

	private static Pair<String, Object> parsingTestCase(final String givenPath, final Object expectedResult)
	{
		return Pair.of(Objects.requireNonNull(givenPath), Objects.requireNonNull(expectedResult));
	}

	@Test
	public void shouldPassAllAssemblingTestCases()
	{
		final DefaultPrettyUrlStrategy givenStrategy = new DefaultPrettyUrlStrategy();

		verify(ASSEMBLING_TEST_CASES, givenStrategy::assemblePath);
	}

	@Test
	public void shouldPassAllParsingTestCases()
	{
		final DefaultPrettyUrlStrategy givenStrategy = new DefaultPrettyUrlStrategy();

		verify(PARSING_TEST_CASES, givenStrategy::parsePath);
	}

	private <T> void verify(final Pair<? extends T, ?>[] testCases, final Function<? super T, Object> getActual)
	{
		final StringBuilder failures = new StringBuilder();

		for (final Pair<? extends T, ?> testCase : testCases)
		{
			final Object expected = testCase.getRight();
			final T testInput = testCase.getLeft();
			try
			{
				final Object actual = getActual.apply(testInput);
				if (!expected.equals(actual))
				{
					failures.append(
							"Expected `" + expected + "` for `" + testInput + "` but `" + actual + "` has been returned.\n");
				}
			}
			catch (final Exception e)
			{
				if (!e.getClass().equals(expected))
				{
					failures.append("Expected `" + expected + "` for `" + testInput + "` but `" + e + "` has been thrown.\n");
				}
			}
		}

		assertThat(failures.toString()).isEmpty();
	}
}