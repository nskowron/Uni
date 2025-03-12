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
package eu.jpereira.trainings.designpatterns.creational.abstractfactory;

import java.util.HashMap;
import java.util.Map;

import eu.jpereira.trainings.designpatterns.creational.abstractfactory.json.JSONReportBody;
import eu.jpereira.trainings.designpatterns.creational.abstractfactory.json.JSONReportFooter;
import eu.jpereira.trainings.designpatterns.creational.abstractfactory.json.JSONReportHeader;
import eu.jpereira.trainings.designpatterns.creational.abstractfactory.xml.XMLReportBody;
import eu.jpereira.trainings.designpatterns.creational.abstractfactory.xml.XMLReportFooter;
import eu.jpereira.trainings.designpatterns.creational.abstractfactory.xml.XMLReportHeader;

public class Report {

	private String reportContent;
	private ReportBody body;
	private ReportFooter footer;
	private ReportHeader header;
	private String reportType;
	

	public Report(String type, Map<String, ReportFactory> factories)
	{
		this.reportType = type;

		ReportFactory factory = factories.get(reportType);
		if(factory == null)
		{
			factory = factories.get("XML");
		}

		this.setBody(factory.buildBody());
		this.setFooter(factory.buildFooter());
		this.setHeader(factory.buildHeader());
	}
	
	/**
	 * @param string
	 */
	public Report(String string)
	{
		this(string, getDefaultFactories());
	}

	private static Map<String, ReportFactory> getDefaultFactories()
	{
		Map<String, ReportFactory> defaultFactories = new HashMap<>();

		defaultFactories.put("JSON", new JSONReportFactory());
		defaultFactories.put("XML", new XMLReportFactory());

		return defaultFactories;
	}


	public void setBody(ReportBody body) {
		this.body = body;

	}

	
	public void setFooter(ReportFooter footer) {
		this.footer = footer;

	}

	
	public void setHeader(ReportHeader header) {
		this.header = header;
	}

	public void setReportContent(String reportContent) {
		this.reportContent = reportContent;
	}


	public String getReportContent() {
		return reportContent;
	}


	public ReportBody getBody() {
		return body;
	}


	public ReportFooter getFooter() {
		return footer;
	}


	public ReportHeader getHeader() {
		return header;
	}

	
}
