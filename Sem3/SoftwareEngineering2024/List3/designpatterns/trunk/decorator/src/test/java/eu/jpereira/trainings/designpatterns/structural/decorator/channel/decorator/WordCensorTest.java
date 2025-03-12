package eu.jpereira.trainings.designpatterns.structural.decorator.channel.decorator;

import static org.junit.Assert.assertEquals;

import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;

import org.junit.Test;

import eu.jpereira.trainings.designpatterns.structural.decorator.channel.SocialChannel;
import eu.jpereira.trainings.designpatterns.structural.decorator.channel.SocialChannelBuilder;
import eu.jpereira.trainings.designpatterns.structural.decorator.channel.SocialChannelProperties;
import eu.jpereira.trainings.designpatterns.structural.decorator.channel.SocialChannelPropertyKey;
import eu.jpereira.trainings.designpatterns.structural.decorator.channel.spy.TestSpySocialChannel;

/**
 * @author jpereira
 */
public class WordCensorTest extends AbstractSocialChanneldDecoratorTest {

    @Test
    public void testCensorWords() {
        SocialChannelBuilder builder = createTestSpySocialChannelBuilder();

        SocialChannelProperties props = new SocialChannelProperties()
                .putProperty(SocialChannelPropertyKey.NAME, TestSpySocialChannel.NAME);

        List<String> censored = new ArrayList<>();
        censored.add("Gorilla");
        censored.add("Godzilla");
        SocialChannel channel = builder.with(new WordCensor(censored)).getDecoratedChannel(props);

        channel.deliverMessage("Gorilla would beat Godzilla anytime!");
        
        TestSpySocialChannel spyChannel = (TestSpySocialChannel) builder.buildChannel(props);
        assertEquals("### would beat ### anytime!", spyChannel.lastMessagePublished());
    }

    @Test
    public void testCensorWordCaseInsensitive() {
        SocialChannelBuilder builder = createTestSpySocialChannelBuilder();

        SocialChannelProperties props = new SocialChannelProperties()
                .putProperty(SocialChannelPropertyKey.NAME, TestSpySocialChannel.NAME);

        List<String> censored = new ArrayList<>();
        censored.add("Gorilla");
        censored.add("Godzilla");
        SocialChannel channel = builder.with(new WordCensor(censored)).getDecoratedChannel(props);

        channel.deliverMessage("I HATE GODZILLA");
        
        TestSpySocialChannel spyChannel = (TestSpySocialChannel) builder.buildChannel(props);
        assertEquals("I HATE ###", spyChannel.lastMessagePublished());
    }

    @Test
    public void testCensorWordPlural() {
        SocialChannelBuilder builder = createTestSpySocialChannelBuilder();

        SocialChannelProperties props = new SocialChannelProperties()
                .putProperty(SocialChannelPropertyKey.NAME, TestSpySocialChannel.NAME);

        List<String> censored = new ArrayList<>();
        censored.add("Gorilla");
        censored.add("Godzilla");
        SocialChannel channel = builder.with(new WordCensor(censored)).getDecoratedChannel(props);

        channel.deliverMessage("is it gorillas or gorillaz?");
        
        TestSpySocialChannel spyChannel = (TestSpySocialChannel) builder.buildChannel(props);
        assertEquals("is it ###s or ###z?", spyChannel.lastMessagePublished());
    }
}
