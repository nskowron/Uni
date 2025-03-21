/**
 * Copyright 2011 Joao Miguel Pereira
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package eu.jpereira.trainings.designpatterns.creational.factorymethod;

import java.util.Map;
import java.util.HashMap;

/**
 * The Report Generator will create reports based on a given type
 * @author jpereira
 *
 */
public class ReportGenerator {

	private Map<String, ReportFactory> factories;

	public ReportGenerator(Map<String, ReportFactory> factories)
	{
		this.factories = factories;
	}

	public ReportGenerator()
	{
		this(getDefaultFactories());
	}

	private static Map<String, ReportFactory> getDefaultFactories()
	{
		Map<String, ReportFactory> defaultFactories = new HashMap<>();

		defaultFactories.put("JSON", new JSONReportFactory());
		defaultFactories.put("XML", new XMLReportFactory());
		defaultFactories.put("HTML", new HTMLReportFactory());
		defaultFactories.put("PDF", new PDFReportFactory());

		return defaultFactories;
	}

	/**
	 * Generate a new report.
	 * @param data The report data
	 * @param type the type of report
	 * @return the generated report, or null of type is unknown
	 */
	public Report generateReport(ReportData data, String type)
	{
		Report report = null;
		ReportFactory factory = factories.get(type);

		if(factory != null)
		{
			report = factory.buildReport();
			report.generateReport(data);
		}

		return report;
	}
}
