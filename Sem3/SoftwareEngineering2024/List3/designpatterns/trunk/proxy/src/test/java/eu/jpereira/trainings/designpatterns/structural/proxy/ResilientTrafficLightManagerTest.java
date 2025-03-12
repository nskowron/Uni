package eu.jpereira.trainings.designpatterns.structural.proxy;

import org.junit.Before;

import eu.jpereira.trainings.designpatterns.structural.proxy.fakes.FakeResilientTrafficLightFactory;
import eu.jpereira.trainings.designpatterns.structural.proxy.testconfig.TestConfiguration;

public class ResilientTrafficLightManagerTest extends TrafficLightsManagerTest
{
    @Before
    public void setUp() {
        TestConfiguration.fakeFailuresInController = true;
    }

    @Override
    protected TrafficLightsManager createTrafficLightsManager() {
        TrafficLightsManager manager = new TrafficLightsManager(new FakeResilientTrafficLightFactory());
        return manager;
    }
}
