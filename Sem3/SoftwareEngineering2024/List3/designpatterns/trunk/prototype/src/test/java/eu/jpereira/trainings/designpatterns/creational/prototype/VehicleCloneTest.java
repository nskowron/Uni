package eu.jpereira.trainings.designpatterns.creational.prototype;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import org.junit.Test;

import eu.jpereira.trainings.designpatterns.creational.prototype.exception.CannotHaveZeroPartsException;
import eu.jpereira.trainings.designpatterns.creational.prototype.exception.CouldNotCloneLastObjectException;
import eu.jpereira.trainings.designpatterns.creational.prototype.exception.NeedToPackLastVehicleException;
import eu.jpereira.trainings.designpatterns.creational.prototype.exception.VehicleDoesNotHavePartsException;
import eu.jpereira.trainings.designpatterns.creational.prototype.model.Shell;
import eu.jpereira.trainings.designpatterns.creational.prototype.model.Tire;
import eu.jpereira.trainings.designpatterns.creational.prototype.model.Vehicle;
import eu.jpereira.trainings.designpatterns.creational.prototype.model.Window;

public class VehicleCloneTest
{
    @Test
    public void testCloneVehicleWithParts() throws NeedToPackLastVehicleException, VehicleDoesNotHavePartsException, CouldNotCloneLastObjectException, CannotHaveZeroPartsException
    {
        SimpleVehicleBuilder builder = new SimpleVehicleBuilder();
		Vehicle vehicle = builder.createVehicle().with(new Tire()).times(10).with(new Window()).times(2).with(new Shell()).packIt();
        Vehicle clone = (Vehicle)vehicle.clone();

        assertEquals(vehicle, clone);
        assertFalse(vehicle == clone);
    }

    @Test
    public void testCloneVehicleWithoutParts() throws NeedToPackLastVehicleException, VehicleDoesNotHavePartsException, CouldNotCloneLastObjectException, CannotHaveZeroPartsException
    {
        SimpleVehicleBuilder builder = new SimpleVehicleBuilder();
		Vehicle vehicle = builder.createVehicle().packIt();
        Vehicle clone = (Vehicle)vehicle.clone();

        assertEquals(vehicle, clone);
        assertFalse(vehicle == clone);
    }
}
