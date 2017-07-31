******
README
******


This project examines the effects of negative social information and pro-clean meat appeals on interest in clean meat products. 

* Link to github: https://github.com/bnjmacdonald/antinaturalistic_fallacy
* Link to documentation: <NA>

All of the code for data analysis is stored in ``analysis``. Data files are in ``data``. These files contain the merged, cleaned, and deidentified, produced from ``bin/clean.sh`` which executes ``cleaning_wave1.r``, ``cleaning_wave2.r``, ``cleaning_wave3.r``, and ``merging_waves.r``.

The main analysis script is ``main_analysis.r``. 

The paper (``paper/paper.html``) is written in ``html + css`` using `pubcss <https://github.com/thomaspark/pubcss>`_. To convert it to pdf, you will need to download `prince <https://www.princexml.com/>`_.

Once Prince is installed, open a new shell session then navigate to the ``paper/`` directory and run::
  
  $ prince antinaturalistic-fallacy.html -o antinaturalistic-fallacy.pdf --javascript

Alternatively, if you wish to re-generate all of the Figures and results reported in the paper, open a new shell session and run the ``bin/build_paper.sh`` script.

You can also view ``paper.html`` in any web browser, although the formatting will not be great.

