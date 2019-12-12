Note: this file is auto converted from company-posframe.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!


# &#30446;&#24405;

1.  [company-posframe README](#orge06ae94)
    1.  [What is company-posframe](#org68ae027)
    2.  [How to use company-posframe](#orgf004a70)
    3.  [Tips](#orgdfa33d0)
        1.  [Work better with desktop.el](#org1a7e07d)
    4.  [Note](#org3b52f89)


<a id="orge06ae94"></a>

# company-posframe README


<a id="org68ae027"></a>

## What is company-posframe

company-posframe is a company extension, which let company use
child frame as its candidate menu.

It has the following feature:

1.  It is fast enough for daily use.
2.  It works well with CJK language.

**At the moment**, company-posframe can not work well with:

1.  company-quickhelp


<a id="orgf004a70"></a>

## How to use company-posframe

    (require 'company-posframe)
    (company-posframe-mode 1)


<a id="orgdfa33d0"></a>

## Tips


<a id="org1a7e07d"></a>

### Work better with desktop.el

The below code let desktop.el not record the company-posframe-mode

    (require 'desktop) ;this line is needed.
    (push '(company-posframe-mode . nil)
          desktop-minor-mode-table)


<a id="org3b52f89"></a>

## Note

company-posframe.el is derived from Clément Pit-Claudel's
company-tooltip.el, which can be found at:

<https://github.com/company-mode/company-mode/issues/745#issuecomment-357138511>

