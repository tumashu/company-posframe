Note: this file is auto converted from company-posframe.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!


# &#30446;&#24405;

1.  [company-posframe README](#org8b8241b)
    1.  [What is company-posframe](#org1aa5d0a)
    2.  [How to use company-posframe](#org5069dd3)
    3.  [Tips](#org6a8909f)
        1.  [Work better with desktop.el](#org02d8a03)
        2.  [Work better with company-quickhelp](#org9967701)
    4.  [Note](#org947ef3a)


<a id="org8b8241b"></a>

# company-posframe README


<a id="org1aa5d0a"></a>

## What is company-posframe

company-posframe is a company extension, which let company use
child frame as its candidate menu.

It has the following feature:

1.  It is fast enough for daily use.
2.  It works well with CJK language.

**At the moment**, company-posframe can not work well with:

1.  company-quickhelp


<a id="org5069dd3"></a>

## How to use company-posframe

    (require 'company-posframe)
    (company-posframe-mode 1)


<a id="org6a8909f"></a>

## Tips


<a id="org02d8a03"></a>

### Work better with desktop.el

The below code let desktop.el not record the company-posframe-mode

    (require 'desktop) ;this line is needed.
    (push '(company-posframe-mode . nil)
          desktop-minor-mode-table)


<a id="org9967701"></a>

### Work better with company-quickhelp

    (require 'company-quickhelp)
    (require 'company-posframe-quickhelp)


<a id="org947ef3a"></a>

## Note

company-posframe.el is derived from Clément Pit-Claudel's
company-tooltip.el, which can be found at:

<https://github.com/company-mode/company-mode/issues/745#issuecomment-357138511>

