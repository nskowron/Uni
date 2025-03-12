package eu.jpereira.trainings.designpatterns.creational.builder.json;

import java.util.Iterator;

import eu.jpereira.trainings.designpatterns.creational.builder.ReportBodyBuilder;
import eu.jpereira.trainings.designpatterns.creational.builder.model.Report;
import eu.jpereira.trainings.designpatterns.creational.builder.model.ReportBody;
import eu.jpereira.trainings.designpatterns.creational.builder.model.SaleEntry;
import eu.jpereira.trainings.designpatterns.creational.builder.model.SoldItem;

public class JSONReportBodyBuilder implements ReportBodyBuilder
{
    @Override
    public ReportBody buildReportBody(SaleEntry saleEntry)
    {
        JSONReportBody reportBody = new JSONReportBody();
        //Add customer info
        reportBody.addContent("sale:{customer:{");
        reportBody.addContent("name:\"");
        reportBody.addContent(saleEntry.getCustomer().getName());
        reportBody.addContent("\",phone:\"");
        reportBody.addContent(saleEntry.getCustomer().getPhone());
        reportBody.addContent("\"}");
        //add array of items
        reportBody.addContent(",items:[");
        Iterator<SoldItem> it = saleEntry.getSoldItems().iterator();
        while ( it.hasNext() ) {
            SoldItem item = it.next();
            reportBody.addContent("{name:\"");
            reportBody.addContent(item.getName());
            reportBody.addContent("\",quantity:");
            reportBody.addContent(String.valueOf(item.getQuantity()));
            reportBody.addContent(",price:");
            reportBody.addContent(String.valueOf(item.getUnitPrice()));
            reportBody.addContent("}");
            if ( it.hasNext() ) {
                reportBody.addContent(",");
            }
            
        }
        reportBody.addContent("]}");

        return reportBody;
    }
}
