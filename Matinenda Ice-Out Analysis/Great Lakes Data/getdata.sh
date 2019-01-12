#!/bin/bash

for i in 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 
do
echo "https://coastwatch.glerl.noaa.gov/ftp/glsea/avgtemps/$i/glsea-temps"$i"_1024.dat > $i.dat"
curl https://coastwatch.glerl.noaa.gov/ftp/glsea/avgtemps/$i/glsea-temps"$i"_1024.dat > $i.dat
done
