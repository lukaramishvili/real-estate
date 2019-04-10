# real-estate

This is a web-app written in Common Lisp for a real-estate company.

It's mainly useful because it uses all the libraries and features that a typical web-app would use (templating, database access, encryption, smtp, image resizing, etc).

Features:
* ORM with paging and filters (powered by PostgreSQL and the `postmodern` library)
* Masonry-like (but horizontal) infinite grid system
* User Interface Localization (L10n)
* Multi-language content support
* Pretty URLs
* Template engine
* Client-side rendering (this was 2012, before React and SPAs)
* Styled admin page
* Standard pages like FAQ/Contact
* Users & Groups (roles) with ACL
* Authentication/Registration with email/OAuth
* Profile/Password management
* User management
* Multiple file upload
* Transpiled Javascript (via parenscript)
* Google Maps multiple marker editor
* QR code generation
* String helpers
* Financial calculator forms and formulas
* Query string parsing
* Date formatting
* Image manipulation (resizing, cropping, letterboxing etc)
* Simplified email sending
* RSS
* Test data generator
* Form generation
* HTTP Cache control
* Guards/Middlewares (e.g. for login)
* tmp file management
* Multiple operating system support (Windows, Linux, MacOS)

Uses:
* hunchentoot for web server
* cl-who for templating
* ironclad for encryption
* cl-gd for image manipulation
* cl-smtp for sending emails
* cl-jpeg for image resizing
* cl-fad for filesystem access
* parenscript for Javascript macros and transpiling to Javascript
* css-lite for CSS
