# LibDetect
LibDetect identifies libraries in Android apps (APK) on the class level and provied a similarity score 
of the class under analysis and a reference class stored in LibDetect's database.

## Precondition
checkout and compile OPAL develop branch (http://www.opal-project.de/)<br>
install OpenSSL (https://www.openssl.org/)<br>
install enjarify (https://github.com/google/enjarify)<br>
install MySQL (https://www.mysql.com/de/downloads/)<br>
execute createDB.sql<br>
configure config.txt<br>
- set USER_NAME to the user who has access to the androidlib database described in createDB
- set PASSWD to the password of the user who has access to the androidlib database described in createDB
- set DB_URL to the url of the internal mysql connection of the androidlib database often it looks like this jdbc:mysql://localhost:3306/androidlib
- set OPENSSL_EXEC_CMD to the operating-system-dependent oppenssl-command in windows it looks like this cmd /c path/to/the/openssl/command/openssl.exe
- set OPENSSL_CONF_PATH to the path of the OpenSSL configuration file 
- set ENJARIFY_EXEC_CMD to the execution path of enjarify in windows it looks like this cmd /c path/to/enjarify/command/enjarify.bat

fill the database with all downloaded JARs which were described in LibDetectDatabase.txt
LibDetectDatabase.txt is structured in the following way.
- Files which were downloaded from maven have a maven prefix.
- Files which were downloaded by a previous google search have a google prefix.
- Files which were downloaded from Fdroid have a fdroid prefix.
- Files which were downloaded from a github search have a github prefix.
- Files which were downloaded via link have a direct link to the source.

# CodeMatch
CodeMatch extracts libraries, checks the size, checks the author, checks if generated code is used, 
and compares the similarity score of apps.
Thereby CodeMatch uses the algorithms of https://github.com/openpreserve/bitwiser and/or 
http://publica.fraunhofer.de/documents/N-264291.html
While the algorithm of Bitwiser is integrated in CodeMatch, 
F2S2 is under a license of the Fraunhofer SIT (https://www.sit.fraunhofer.de/) and can be obtained from the authors 
of the paper http://publica.fraunhofer.de/documents/N-264291.html