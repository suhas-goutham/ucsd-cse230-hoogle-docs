# ucsd-cse230-hoogle-docs
CSE 230 project - Hoogle Docs (Google docs in Haskell)
## Overview
In this project, we aim to implement a subset of the Google Docs functionality on Haskell using the [brick](https://github.com/jtdaugherty/brick/) library interface. 
## Goals
We have 3 primary goals:
  1. Shared Editing - Using this feature, multiple users can edit the document synchronously. We will implement the shared interface using websockets.
  2. Commenting - This feature enables any user to select a portion of text in the editor and leave a comment that can be viewed by all users. The owner of the document can mark a comment as resolved, if they choose to, thereby closing the comment.
  3. Track Changes - This feature allows users to look at the history of the document, including edits, and whom the edits were made by along with timestamps for the same. This feature also allows the owner of the document to restore any previous version of the document. Historical document versions will also contain the comments left in previous versions (ambitious).
  4. (Ambitious) Editing and Suggesting modes - If time permits, we will also try to implement a separate suggest mode in the document. This feature would enable a user to suggest changes in the document, which can be accepted or rejected by the owner of the document.
## Limitations
Due to the large scope of the project, we will be consciously cutting back on some Google Doc functionality. Some significant limitations include:
  1. User roles - In our version of the project, each document will have a single owner who can perform document restorations, and (if implemented) accept and reject suggestions. Additionally, the owner will have the ability to resolve and close comments. We may not have the time to implement different user roles, such as Editors and Viewers. Therefore, all users will be able to edit, and comment on a document that they are added to.
  2. Editor GUI - The editor GUI and capabilities may not be as rich as the Google Docs interface due to time constraints. With shared editing and document history as our central goal, we will attempt to make the editor as nice as possible.
