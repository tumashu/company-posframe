
# &#30446;&#24405;

1.  [company-childframe README](#org3ce19e1)
    1.  [What is company-childframe](#orgff8d689)
    2.  [How to use company-childframe](#orgf75576c)
    3.  [Tips](#org4da50bd)
        1.  [Work better with desktop.el](#org0eaba34)
    4.  [Note](#orgd2315ed)


<a id="org3ce19e1"></a>

# company-childframe README


<a id="orgff8d689"></a>

## What is company-childframe

company-childframe is a company extension, which let company use
child frame as its candidate menu.

It has the following feature:

1.  It is fast enough for daily use.
2.  It works well with CJK language.

**At the moment**, company-childframe can not work well with:

1.  company-quickhelp


<a id="orgf75576c"></a>

## How to use company-childframe

    (require 'company-childframe)
    (company-childframe-mode 1)


<a id="org4da50bd"></a>

## Tips


<a id="org0eaba34"></a>

### Work better with desktop.el

The below code let desktop.el not record the company-childframe-mode

    (require 'desktop) ;this line is needed.
    (push '(company-childframe-mode . nil)
          desktop-minor-mode-table)


<a id="orgd2315ed"></a>

## Note

company-childframe.el is derived from Clément Pit-Claudel's
company-tooltip.el, which can be found at:

<https://github.com/company-mode/company-mode/issues/745#issuecomment-357138511>



Converted from company-childframe.el by [el2org](https://github.com/tumashu/el2org) .