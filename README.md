Note: this file is auto converted from company-posframe.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!


# &#30446;&#24405;

1.  [company-posframe README](#org07182e1)
    1.  [What is company-posframe](#org35f7f92)
    2.  [How to use company-posframe](#org6ecb9f3)
    3.  [Tips](#orgb340efe)
        1.  [How to reduce flicker when scroll up and down?](#orge2be77f)
        2.  [Work better with desktop.el](#orgaa10af6)
    4.  [Note](#org317fc05)


<a id="org07182e1"></a>

# company-posframe README


<a id="org35f7f92"></a>

## What is company-posframe

company-posframe is a company extension, which let company use
child frame as its candidate menu.

It has the following feature:

1.  It is fast enough for daily use.
2.  It works well with CJK language.

![img](./snapshots/company-posframe.png)


<a id="org6ecb9f3"></a>

## How to use company-posframe

    (require 'company-posframe)
    (company-posframe-mode 1)


<a id="orgb340efe"></a>

## Tips


<a id="orge2be77f"></a>

### How to reduce flicker when scroll up and down?

In windows or MacOS system, company candidates menu may flicker
when scroll up and down, the reason is that the size of posframe
changing rapid, user can set the minimum width of menu to limit
flicker, for example:

    (setq company-tooltip-minimum-width 40)


<a id="orgaa10af6"></a>

### Work better with desktop.el

The below code let desktop.el not record the company-posframe-mode

    (require 'desktop) ;this line is needed.
    (push '(company-posframe-mode . nil)
          desktop-minor-mode-table)


<a id="org317fc05"></a>

## Note

company-posframe.el is derived from Clément Pit-Claudel's
company-tooltip.el, which can be found at:

<https://github.com/company-mode/company-mode/issues/745#issuecomment-357138511>

Some quickhelp functions is come from:

[company-quickhelp](https://github.com/company-mode/company-quickhelp)

