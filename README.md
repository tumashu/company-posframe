
# &#30446;&#24405;

1.  [company-childframe README](#org3af03de)
    1.  [What is company-childframe](#org175b976)
    2.  [How to use company-childframe](#org2db81e7)
    3.  [Tips](#orgd28e830)
        1.  [Work better with desktop.el](#org80ed06c)
    4.  [Note](#org2d583c3)


<a id="org3af03de"></a>

# company-childframe README


<a id="org175b976"></a>

## What is company-childframe

company-childframe is a company extension, which let company use
child frame as its candidate menu.

It has the following feature:

1.  It is fast enough for daily use.
2.  It works well with CJK language.


<a id="org2db81e7"></a>

## How to use company-childframe

    (require 'company-childframe)
    (company-childframe-mode 1)


<a id="orgd28e830"></a>

## Tips


<a id="org80ed06c"></a>

### Work better with desktop.el

The below code let desktop.el not record the company-childframe-mode

    (require 'desktop) ;this line is needed.
    (push '(company-childframe-mode . nil)
          desktop-minor-mode-table)


<a id="org2d583c3"></a>

## Note

company-childframe.el is derived from Clément Pit-Claudel's
company-tooltip.el, which can be found at:

<https://github.com/company-mode/company-mode/issues/745#issuecomment-357138511>



Converted from company-childframe.el by [el2org](https://github.com/tumashu/el2org) .