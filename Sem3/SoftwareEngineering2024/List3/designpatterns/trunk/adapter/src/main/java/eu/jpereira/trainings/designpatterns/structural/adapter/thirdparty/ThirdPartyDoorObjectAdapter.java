package eu.jpereira.trainings.designpatterns.structural.adapter.thirdparty;

import eu.jpereira.trainings.designpatterns.structural.adapter.exceptions.CodeMismatchException;
import eu.jpereira.trainings.designpatterns.structural.adapter.exceptions.IncorrectDoorCodeException;
import eu.jpereira.trainings.designpatterns.structural.adapter.model.Door;
import eu.jpereira.trainings.designpatterns.structural.adapter.thirdparty.ThirdPartyDoor.DoorState;
import eu.jpereira.trainings.designpatterns.structural.adapter.thirdparty.exceptions.CannotChangeCodeForUnlockedDoor;
import eu.jpereira.trainings.designpatterns.structural.adapter.thirdparty.exceptions.CannotChangeStateOfLockedDoor;
import eu.jpereira.trainings.designpatterns.structural.adapter.thirdparty.exceptions.CannotUnlockDoorException;

public class ThirdPartyDoorObjectAdapter implements Door
{
    private ThirdPartyDoor thirdPartyDoorObject = new ThirdPartyDoor();

    @Override
    public void open(String code) throws IncorrectDoorCodeException
    {
        try
        {
            thirdPartyDoorObject.unlock(code);
            thirdPartyDoorObject.setState(DoorState.OPEN);
        }
        catch(CannotUnlockDoorException | CannotChangeStateOfLockedDoor e)
        {
            throw new IncorrectDoorCodeException();
        }
    }

    @Override
    public void close()
    {
        try
        {
            thirdPartyDoorObject.setState(DoorState.CLOSED);
            thirdPartyDoorObject.lock();
        }
        catch(CannotChangeStateOfLockedDoor e){}
    }

    @Override
    public boolean isOpen()
    {
        return thirdPartyDoorObject.getState() == DoorState.OPEN;
    }

    @Override
    public void changeCode(String oldCode, String newCode, String newCodeConfirmation)
            throws IncorrectDoorCodeException, CodeMismatchException
    {
        if(!newCode.equals(newCodeConfirmation))
        {
            throw new CodeMismatchException();
        }
        try
        {
            thirdPartyDoorObject.unlock(oldCode);
            thirdPartyDoorObject.setNewLockCode(newCode);
            thirdPartyDoorObject.lock();
        }
        catch(CannotUnlockDoorException | CannotChangeCodeForUnlockedDoor e)
        {
           throw new IncorrectDoorCodeException();
        }
    }

    @Override
    public boolean testCode(String code)
    {
        try
        {
            thirdPartyDoorObject.unlock(code);
            thirdPartyDoorObject.lock();
            return true;
        }
        catch(CannotUnlockDoorException e)
        {
            return false;
        }
    }
}
