# Changelog for GramHs

## 1.0.3

### Feautres added:

  - Click again on a selected friend/group to move it to the top of the list.

## 1.0.2

### Adjustments: 

  - Moved to lts-17.4.

### Feautres added:

  - Sort member list.

## 1.0.1

### Adjustments: 

  - Renamed to GramHs.

### Bugs fixed:

  - Couldn't read `config.json`. Now it's moved to `/etc/gramhs.json`.

  - Some escape characters couldn't be shown properly.

### Feautres added:

  - Quoting function: Now you can quote and reply to other messages.

  - A setup script for Mirai.

### Known Bugs:

  - When Mirai failed to send a message, GramHs may fail to parse the response and stuck.

## 1.0.0

### Description: 

Basic features implemented.

### Future:

  - Add quoting function.

  - Add temporary conversation.

### Known Bugs:

  - None.

## Unreleased Changes

  - Improve the parser for input. Now `[` no nonger needs to be escaped.
