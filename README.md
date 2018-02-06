
# &#30446;&#24405;

1.  [company-childframe README](#org4663135)
    1.  [What is company-childframe](#org59ce0d0)
    2.  [How to use company-childframe](#orgda40d2a)
    3.  [Tips](#org63199bc)
        1.  [Work better with desktop.el](#orgd6ea75f)
    4.  [Note](#org593ae90)


<a id="org4663135"></a>

# company-childframe README


<a id="org59ce0d0"></a>

## What is company-childframe

company-childframe is a company extension, which let company use
child frame as its candidate menu.

It has the following feature:

1.  It is more fast than the company default candidate menu.
2.  It works well with CJK language.


<a id="orgda40d2a"></a>

## How to use company-childframe

    (require 'company-childframe)
    (company-childframe-mode 1)


<a id="org63199bc"></a>

## Tips


<a id="orgd6ea75f"></a>

### Work better with desktop.el

The below code let desktop.el not record the company-childframe-mode

    (require 'desktop) ;this line is needed.
    (push '(company-childframe-mode . nil)
          desktop-minor-mode-table)


<a id="org593ae90"></a>

## Note

company-childframe.el is derived from Clément Pit-Claudel's
company-tooltip.el, which can be found at:

<https://github.com/company-mode/company-mode/issues/745#issuecomment-357138511>



Converted from company-childframe.el by [el2org](https://github.com/tumashu/el2org) .