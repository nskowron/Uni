package eu.jpereira.trainings.designpatterns.creational.factorymethod;

import java.util.HashMap;
import java.util.Map;

interface ReportFactory
{
    Report buildReport();
}

class JSONReportFactory implements ReportFactory
{
    @Override
    public Report buildReport()
    {
        return new JSONReport();
    }
}

class XMLReportFactory implements ReportFactory
{
    @Override
    public Report buildReport()
    {
        return new XMLReport();
    }
}

class HTMLReportFactory implements ReportFactory
{
    @Override
    public Report buildReport()
    {
        return new HTMLReport();
    }
}

class PDFReportFactory implements ReportFactory
{
    @Override
    public Report buildReport()
    {
        return new PDFReport();
    }
}
