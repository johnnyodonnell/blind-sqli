## blind-sqli

Offensive Security has a policy that restricts the use of 'sqlmap'
for any of their certification exams. 'sqlmap' is a tool that is
used to facilitate SQL injection on vulnerable web applications.
Understandably, the reason that Offensive Security restricts the
use of 'sqlmap' is to ensure that their students understand how
SQL injection actually works.

The primary downside of this policy though is that executing blind
SQL injections with custom scripts can require a lot of effort.
For a lab machine that I hacked in preparation for the OSCP exam,
I wrote custom blind SQL injection scripts that amounted to nearly
500 lines of code. These scripts listed all of the databases in
the server, listed all of the tables for a given database, and
dumped the contents of a given table.

Now that I am working towards my OSWE, I have decided to write my
own blind SQL injection tool in order to (1) prove that I understand
how blind SQL injection actually works and to (2) save myself
the time and effort of writing custom scripts for each individual
machine that contains a SQLi vulnerability.

This tool will be written in Racket, my favorite programming language.

### Limitations

* Only works for error-based blind SQL injection
* Only works on MySQL databases

### Other Notes

* Heavily inspired by the SQLi scripts I wrote for 10.1.1.246
    from the OSCP labs.
    Private repository link: https://github.com/johnnyodonnell/notes-on-oscp-lab-machines/tree/master/10.1.1.246
* Example command for 10.1.1.246 from the OSCP labs:\
    `racket blind-sql.rkt -d wordpress -t wp_users -s "<results>.+</results>" dump-table GET 10.11.1.251 /wp/wp-content/plugins/wp-autosuggest/autosuggest.php "wpas_action=query&wpas_keys=1'/**/OR/**/~a);#"`

