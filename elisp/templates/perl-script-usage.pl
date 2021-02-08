use constant _ME_ => $0 =~ m=([^/]+)$=;

if((@ARGV == 0) || ($ARGV[0] =~ /(?^:^(?:-(?:(?:-(?:h(?:elp)?|\?)|h(?:elp)?)$|\?)|\?$))/))
{
  say "\nUsage: $me\n";
  exit;
}
