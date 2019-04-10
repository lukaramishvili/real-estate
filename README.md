# real-estate

This is a web-app written in Common Lisp for a real-estate company.

It's mainly useful because it uses all the libraries and features that a typical web-app would use (templating, database access, encryption, smtp, image resizing, etc).

Features:
* ORM with paging and filters
* Masonry-like (but horizontal) infinite grid system
* Pretty URLs
* Template engine
* Client-side rendering (this was 2012, before React and SPAs)
* Styled admin page
* Standard pages like FAQ/Contact
* User Interface Localization (L10n)
* Multi-language content
* Users & Groups (roles) with ACL
* Authentication/Registration with email/OAuth
* Profile/Password management
* User management
* Transpiled Javascript (via parenscript)
* Google Maps multiple marker editor
* QR code generation
* String helpers
* Financial calculator forms and formulas
* Query string parsing
* Date formatting
* Image resizing
* Simplified email sending
* RSS
* Test data generator
* Form generation

Uses:
* cl-smtp for sending emails
* cl-jpeg for image resizing
