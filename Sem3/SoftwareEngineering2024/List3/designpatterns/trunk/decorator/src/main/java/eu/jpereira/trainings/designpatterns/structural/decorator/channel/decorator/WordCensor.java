package eu.jpereira.trainings.designpatterns.structural.decorator.channel.decorator;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class WordCensor extends SocialChannelDecorator
{
    protected List<String> censoredWords;

    public WordCensor(List<String> wordsToCensor)
    {
        censoredWords = wordsToCensor;
    }

    @Override
    public void deliverMessage(String message)
    {
        String censoredMessage = message;
        for(String word : censoredWords)
        {
            Pattern regex = Pattern.compile(word, Pattern.CASE_INSENSITIVE);
            Matcher matcher = regex.matcher(censoredMessage);
            censoredMessage = matcher.replaceAll("###");
        }
        delegate.deliverMessage(censoredMessage);
    }
}
