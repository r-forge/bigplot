
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<p> The idea of this project is simply to create plottable data
structures that are more storage efficient than the data frames which
are the standard inputs to lattice/ggplot2 plot functions.</p>

<p>There are 2 ideas I am currently exploring in this
project. Plotting directly from an external database system, and
plotting based on a list of arrays. Let's see an example of how an
array list can work to plot the same data set more efficiently.</p>

<pre>
install.packages("alplot",repos="http://r-forge.r-project.org")
library(alplot)
data(BodyWeight,package="nlme")
attach(BodyWeight)
al <- 
  arraylist(weight=t(narray(weight,Time,Rat)),
            time=narray(unique(Time),Time),
            Diet=narray(factor(Diet[Time==1]),Rat))
detach(BodyWeight)
excess <- as.numeric(object.size(BodyWeight)/object.size(al))
print(excess)
[1] 2.936364
</pre>

<p>It takes a bit of manual work to convert the data frame to the
correct array list format that is required to plot, but the end result
is that the same data take up 3 times less space. Since all the
information is preserved, we can still plot it using lattice and the
plot method for arraylist objects:</p>

<pre>
xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type="l",layout=c(3,1))
plot(al,weight~time|Diet,groups=Rat,type="l",layout=c(3,1))
</pre>

<img src="long.png" alt="longitudinal data" />

<p>The basic idea behind the arraylist plot method is that we
construct a minimal dummy data frame for passing as input to the usual
xyplot function. Then, for each call to panel.groups, we bring the
real data into memory to plot.</p>

<p>This idea can be extended to use ff arrays or an external sql
database system, instead of array lists. These options have the
potential to be even less taxing on R's memory usage.</p>

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
