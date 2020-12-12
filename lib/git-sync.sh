#!/bin/bash
# sync Jeffs repos to ease multi machine editing.
#  shamelessly derived from http://doc.norang.ca/org-mode.html#git-sync

# Local bare repository name
# syncrepo=norang
# reporoot=~/git

# Log a message for a repository
git_sync_log_msg() {
    printf "  $1\n"
}

# fast-forward reference $ref to $remote/$ref
git_fast_forward_ref() {
    local git_path="$1"
    local remote="$2"
    local ref="$3"
    git_sync_log_msg "fast-forwarding ref $ref"
    current_ref=$(cat $git_path/.git/HEAD)
    if [ "x$current_ref" = "xref: refs/heads/$ref" ]
    then
        # Check for dirty index
        files=$(git -C $git_path diff-index --name-only HEAD --)
        git -C $git_path merge refs/remotes/$remote/$ref
    else
        git -C $git_path branch -f $ref refs/remotes/$remote/$ref
    fi
}

# Push reference $1 to $remote
git_push_ref() {
    local git_path="$1"
    local remote="$2"
    local ref="$3"
    git_sync_log_msg "Pushing ref $ref to $remote"
    if ! git -C $git_path push --tags $remote $ref
    then
        return 1
    fi
}

# Check if a ref can be moved
#   - fast-forwards if behind the sync repo and is fast-forwardable
#   - Does nothing if ref is up to date
#   - Pushes ref to $remote if ref is ahead of remote and fastforwardable
#   - Fails if ref and $syncrop/ref have diverged
git_check_ref() {
    local git_path="$1"
    local remote="$2"
    local ref="$3"
    revlist1=$(git -C $git_path rev-list refs/remotes/$remote/$ref..$ref)
    revlist2=$(git -C $git_path rev-list $ref..refs/remotes/$remote/$ref)
    if [ "x$revlist1" = "x" -a "x$revlist2" = "x" ]
    then
        # Ref $ref is up to date.
        :
    elif [ "x$revlist1" = "x" ]
    then
        # Ref $ref is behind $remote/$ref and can be fast-forwarded.
        git_fast_forward_ref $git_path $remote $ref || return 1
    elif [ "x$revlist2" = "x" ]
    then
        # Ref $ref is ahead of $remote/$ref and can be pushed.
        git_push_ref $git_path $remote $ref || return 1
    else
        git_sync_log_msg "Ref $ref and $remote/$ref have diverged."
        return 1
    fi
}

# Check all local refs with matching refs in the $remote
#  effectively this syncs all tracking branchs
git_check_refs() {
    local git_path="$1"
    local remote="$2"
    git -C $git_path for-each-ref "refs/heads/*" | while read sha1 commit ref
    do
        ref=${ref/refs\/heads\//}
        git -C $git_path for-each-ref refs/remotes/$remote/$ref | while read sha2 commit ref2
        do
            if [ "x$sha2" != "x" -a "x$sha2" != "x" ]
            then
                git_check_ref $git_path $remote $ref || return 1
            fi
        done
    done
}

# sync the remote down to local
git_update_remote() {
    local git_path="$1"
    local remote="$2"
    upd_out=$(git -C $git_path remote update $remote 2>& 1 || return 1)
    git_sync_log_msg "$git_path: $upd_out"
    [ "x$upd_out" = "xFetching $remote" ] || {
        git_sync_log_msg "$git_path: $upd_out"
    }
}

git_sync() {
    local git_path="$1"
    local remote="$2"
    [[ -d $git_path ]] || return 1
    git_update_remote $git_path $remote || return 1
    git_check_refs $git_path $remote || return 1
}


sift_dir() {
    if [[ -e ${HOME}/sift && -d ${HOME}/sift ]]; then
        printf "${HOME}/sift"
    elif [[ -e ${HOME}/pdata/employers/sift && -d ${HOME}/pdata/employers/sift ]]; then
        printf "${HOME}/pdata/employers/sift"
    else
        printf "unknown"
    fi
}

gs_sift_bin() {
    git_sync "$(sift_dir)/sift-bin" origin
}

gs_sift_notes() {
    git_sync "$(sift_dir)/notes" origin
}

gs_sift_todo() {
    git_sync "$(sift_dir)/todo" origin
}

gs_sift_pdata() {
    git_sync "$(sift_dir)/pdata/todo" origin
    git_sync "$(sift_dir)/pdata/notes" origin
}

gs_sift() {
    gs_sift_todo
    gs_sift_notes
    gs_sift_pdata
    gs_sift_bin
}

jwm_dir() {
    printf "${HOME}/jwm"
}

gs_todo() {
    git_sync $(jwm_dir)/todo origin
}

gs_notes() {
    git_sync $(jwm_dir)/notes origin
}

gs_dotfiles() {
    git_sync $(jwm_dir)/proj/jwm-dotfiles origin
    git_sync $(jwm_dir)/proj/jeff-dotfiles origin
}

function gs_jwm_bin {
    git_sync $(jwm_dir)/proj/jwm-bin origin
}

gs_ebooks() {
    git_sync $(jwm_dir)/ebooks origin
}

gs_literate-emacs-d() {
    git_sync $(jwm_dir)/proj/literate-emacs.d origin
}

gs_gcloud_training_2020() {
    git_sync $(jwm_dir)/proj/gcloud-training-2020 origin
}

gs_sql_for_data_analytics() {
    git_sync $(jwm_dir)/proj/sql-for-data-analytics origin
}

gs_course_java_oo_specialization() {
    git_sync $(jwm_dir)/proj/course-oo-java-specialization origin
}


gs_jwm() {
    gs_todo
    gs_notes
    gs_literate-emacs-d
    gs_dotfiles
    gs_jwm_bin
    gs_gcloud_training_2020
    gs_sql_for_data_analytics
    gs_course_java_oo_specialization
    gs_ebooks
}

gs_all() {
    gs_jwm
    gs_sift
}
