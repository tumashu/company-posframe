Note: this file is auto converted from company-posframe.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!


# &#30446;&#24405;

1.  [company-posframe README](#org184ec57)
    1.  [What is company-posframe](#org5977ed1)
    2.  [How to use company-posframe](#org4860ee2)
    3.  [Tips](#org3b5bafe)
        1.  [Work better with desktop.el](#orgaa2ca0f)
    4.  [Note](#org836557d)


<a id="org184ec57"></a>

# company-posframe README


<a id="org5977ed1"></a>

## What is company-posframe

company-posframe is a company extension, which let company use
child frame as its candidate menu.

It has the following feature:

1.  It is fast enough for daily use.
2.  It works well with CJK language.

**At the moment**, company-posframe can not work well with:

1.  company-quickhelp


<a id="org4860ee2"></a>

## How to use company-posframe

    (require 'company-posframe)
    (company-posframe-mode 1)


<a id="org3b5bafe"></a>

## Tips


<a id="orgaa2ca0f"></a>

### Work better with desktop.el

The below code let desktop.el not record the company-posframe-mode

    (require 'desktop) ;this line is needed.
    (push '(company-posframe-mode . nil)
          desktop-minor-mode-table)


<a id="org836557d"></a>

## Note

company-posframe.el is derived from Clément Pit-Claudel's
company-tooltip.el, which can be found at:

<https://github.com/company-mode/company-mode/issues/745#issuecomment-357138511>
