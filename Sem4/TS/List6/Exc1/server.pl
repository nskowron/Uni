    use HTTP::Daemon;
    use HTTP::Status;

    my $d = HTTP::Daemon->new(
            LocalAddr => 'goofball',
            LocalPort => 4322,
        )|| die;
    
    print "Please contact me at: <URL:", $d->url, ">\n";


    while (my $c = $d->accept) {
        while (my $r = $c->get_request) {
            if ($r->method eq 'GET') {
                
                $file_s= "./index.html";    # index.html - jakis istniejacy plik
                $c->send_file_response($file_s);

            }
            else {
                $c->send_error(RC_FORBIDDEN)
            }

        }
        $c->close;
        undef($c);
    }
