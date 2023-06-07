if((@ARGV == 0) || ($ARGV[0] =~ /^(?:-?\?|-h|--help)$/))
{
  say "\nUsage: ",_ME_,"\n";
  exit;
}
