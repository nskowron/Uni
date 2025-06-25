    use HTTP::Daemon;
    use HTTP::Status;

    my $d = HTTP::Daemon->new(
        LocalAddr => 'goofball',
        LocalPort => 4321,
    ) || die;

    print "Please contact me at: <URL:", $d->url, ">\n";


    while (my $c = $d->accept) {
        while (my $r = $c->get_request) {
            my $uri = $r->uri->path;

            # Map "/" to "index.html"
            my $file = $uri eq '/' ? 'index.html' : substr($uri, 1);

            if (-f $file) {
                # Load file
                open(my $fh, '<', $file) or die "Cannot open $file: $!";
                my $content = do { local $/; <$fh> };
                close($fh);

                # Update the number
                if ($content =~ /<h6>Tics: (\d+)<\/h6>/) {
                    my $count = $1 + 1;
                    $content =~ s/<h6>Tics: \d+<\/h6>/<h6>Tics: $count<\/h6>/;
                }
                if ($content =~ /<h6>Tacs: (\d+)<\/h6>/) {
                    my $count = $1 + 1;
                    $content =~ s/<h6>Tacs: \d+<\/h6>/<h6>Tacs: $count<\/h6>/;
                }
                if ($content =~ /<h6>Toes: (\d+)<\/h6>/) {
                    my $count = $1 + 1;
                    $content =~ s/<h6>Toes: \d+<\/h6>/<h6>Toes: $count<\/h6>/;
                }

                # Write the updated content back to the same file
                open(my $fh_out, '>', $file) or die "Cannot write to $file: $!";
                print $fh_out $content;
                close($fh_out);

                $c->send_file_response($file);
            } else {
                $c->send_error(RC_NOT_FOUND);
            }
        }
        $c->close;
        undef($c);
    }
