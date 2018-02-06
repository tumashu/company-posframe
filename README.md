
# &#30446;&#24405;

1.  [company-childframe README](#orgdff7546)
    1.  [What is company-childframe](#org75d8502)
    2.  [How to use company-childframe](#org451f5c4)
    3.  [Tips](#org5148a38)
        1.  [Work better with desktop.el](#org39a885b)
    4.  [Note](#org22e43a8)


<a id="orgdff7546"></a>

# company-childframe README


<a id="org75d8502"></a>

## What is company-childframe

company-childframe is a company extension, which let company use
child frame as its candidate menu.

It has the following feature:

1.  It is more fast than the company default candidate menu.
2.  It works well with CJK language.


<a id="org451f5c4"></a>

## How to use company-childframe

    (require 'company-childframe)
    (company-childframe-mode 1)


<a id="org5148a38"></a>

## Tips


<a id="org39a885b"></a>

### Work better with desktop.el

The below code let desktop.el not record the company-childframe-mode

    (push '(company-childframe-mode . nil)
          desktop-minor-mode-table)


<a id="org22e43a8"></a>

## Note

company-childframe.el is derived from Clément Pit-Claudel's
company-tooltip.el, which can be found at:

<https://github.com/company-mode/company-mode/issues/745#issuecomment-357138511>



Converted from company-childframe.el by [el2org](https://github.com/tumashu/el2org) .