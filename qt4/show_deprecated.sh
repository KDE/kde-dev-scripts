file=$1
test -z "$file" && file=MAKE.OUTPUT

grep -e "is deprecated" $file |sed 's/^.*\/\([-_a-zA-Z0-9]*.h\):[0-9]*)$/\1/g'|sort|uniq -c |sort -n 
echo "";
echo "";

grep -e "is deprecated" $file  |sed 's/^.*warning: .\(.*\). is deprecated.*\/\([-_a-zA-Z0-9]*.h\):[0-9]*)$/\2 - Method: \1/g'|sort|uniq -c |sort -n

echo "---------------------" 
echo "all deprecated method:"
grep -e "is deprecated" $file |wc 
echo "without deprecated methods in generated files:"
grep -e "is deprecated" $file |grep -v /build/ |wc

