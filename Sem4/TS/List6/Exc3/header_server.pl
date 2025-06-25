    use HTTP::Daemon;
    use HTTP::Status;

    my $d = HTTP::Daemon->new(
        LocalAddr => 'goofball',
        LocalPort => 4321,
    )|| die;
    
    print "Please contact me at: <URL:", $d->url, ">\n";


    while (my $c = $d->accept) {
        while (my $r = $c->get_request) {
            my $h = $r->headers_as_string;
            $c->send_response(
                HTTP::Response->new(RC_OK, "OK", ['Content-Type' => 'text/plain'], $h)
            );
        }
        $c->close;
        undef($c);
    }
