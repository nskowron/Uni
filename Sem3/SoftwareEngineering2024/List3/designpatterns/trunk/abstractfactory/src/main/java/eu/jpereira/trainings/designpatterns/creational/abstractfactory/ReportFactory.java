package eu.jpereira.trainings.designpatterns.creational.abstractfactory;

import eu.jpereira.trainings.designpatterns.creational.abstractfactory.json.JSONReportBody;
import eu.jpereira.trainings.designpatterns.creational.abstractfactory.json.JSONReportFooter;
import eu.jpereira.trainings.designpatterns.creational.abstractfactory.json.JSONReportHeader;
import eu.jpereira.trainings.designpatterns.creational.abstractfactory.xml.XMLReportBody;
import eu.jpereira.trainings.designpatterns.creational.abstractfactory.xml.XMLReportFooter;
import eu.jpereira.trainings.designpatterns.creational.abstractfactory.xml.XMLReportHeader;

public interface ReportFactory
{
    public ReportBody buildBody();
    public ReportHeader buildHeader();
    public ReportFooter buildFooter();
}

class JSONReportFactory implements ReportFactory
{
    @Override
    public ReportBody buildBody() {
        return new JSONReportBody();
    }
    
    @Override
    public ReportHeader buildHeader() {
        return new JSONReportHeader();
    }
    
    @Override
    public ReportFooter buildFooter() {
        return new JSONReportFooter();
    }
}

class XMLReportFactory implements ReportFactory
{
    @Override
    public ReportBody buildBody() {
        return new XMLReportBody();
    }
    
    @Override
    public ReportHeader buildHeader() {
        return new XMLReportHeader();
    }
    
    @Override
    public ReportFooter buildFooter() {
        return new XMLReportFooter();
    }
}
