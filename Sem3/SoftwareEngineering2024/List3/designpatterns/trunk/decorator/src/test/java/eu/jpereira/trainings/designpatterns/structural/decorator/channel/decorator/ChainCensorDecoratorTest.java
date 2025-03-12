package eu.jpereira.trainings.designpatterns.structural.decorator.channel.decorator;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import eu.jpereira.trainings.designpatterns.structural.decorator.channel.SocialChannel;
import eu.jpereira.trainings.designpatterns.structural.decorator.channel.SocialChannelBuilder;
import eu.jpereira.trainings.designpatterns.structural.decorator.channel.SocialChannelProperties;
import eu.jpereira.trainings.designpatterns.structural.decorator.channel.SocialChannelPropertyKey;
import eu.jpereira.trainings.designpatterns.structural.decorator.channel.spy.TestSpySocialChannel;

/**
 * Tests chaining multiple decorators, including WordCensor, MessageTruncator, and URLAppender.
 */
public class ChainCensorDecoratorTest extends AbstractSocialChanneldDecoratorTest {

    @Test
    public void testChainWordCensorAndURLAppender() {
        SocialChannelBuilder builder = createTestSpySocialChannelBuilder();

        SocialChannelProperties props = new SocialChannelProperties()
                .putProperty(SocialChannelPropertyKey.NAME, TestSpySocialChannel.NAME);


        List<String> censored = new ArrayList<>();
        censored.add("Camel");
        censored.add("Cabel");
        SocialChannel channel = builder
                .with(new WordCensor(censored))
                .with(new URLAppender("http://example.com"))
                .getDecoratedChannel(props);

        channel.deliverMessage("A Cameleon was dangling on a cabel");
        TestSpySocialChannel spyChannel = (TestSpySocialChannel) builder.buildChannel(props);

        assertEquals("A ###eon was dangling on a ### http://example.com", spyChannel.lastMessagePublished());
    }

    @Test
    public void testChainWordCensorAndMessageTruncator() {
        SocialChannelBuilder builder = createTestSpySocialChannelBuilder();

        SocialChannelProperties props = new SocialChannelProperties()
                .putProperty(SocialChannelPropertyKey.NAME, TestSpySocialChannel.NAME);

        List<String> censored = new ArrayList<>();
        censored.add("Camel");
        censored.add("Cabel");
        SocialChannel channel = builder
                .with(new WordCensor(censored))
                .with(new MessageTruncator(20))
                .getDecoratedChannel(props);

        channel.deliverMessage("camelcabelcamelcabelcamelcabelcamelcabel");
        TestSpySocialChannel spyChannel = (TestSpySocialChannel) builder.buildChannel(props);

        assertEquals("#################...", spyChannel.lastMessagePublished());
    }

    @Test
    public void testChainAllDecorators() {
        SocialChannelBuilder builder = createTestSpySocialChannelBuilder();

        SocialChannelProperties props = new SocialChannelProperties()
                .putProperty(SocialChannelPropertyKey.NAME, TestSpySocialChannel.NAME);

        List<String> censored = new ArrayList<>();
        censored.add("Camel");
        censored.add("Cabel");
        SocialChannel channel = builder
                .with(new WordCensor(censored))
                .with(new URLAppender("http://example.com"))
                .with(new MessageTruncator(25))
                .getDecoratedChannel(props);

        // Deliver the message
        channel.deliverMessage("cameleoncameleon");
        TestSpySocialChannel spyChannel = (TestSpySocialChannel) builder.buildChannel(props);

        assertEquals("###eon###eon http://ex...", spyChannel.lastMessagePublished());
    }
}
