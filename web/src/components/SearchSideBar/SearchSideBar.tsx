import IconButton          from '../IconButton';
import Toggle              from '../Toggle';
import React, { useState } from 'react';
import TextInput           from '../TextInput';
import styles              from './SearchSideBar.module.css';

/* -------------------------------------------------------------------------- */

interface TagProps {
    tag: string;
    category?: string;
}

const Tag = (props: TagProps) => (
    <div className={styles.Tag}>
        <i className="icofont-tag"></i>
        <a href="#" className={styles[`Category--${props.category}`]}>
            {props.tag}
        </a>
    </div>
);

/* -------------------------------------------------------------------------- */

interface Props {
    tags?: string[];
}

const SearchSideBar = (props: Props) => {
    const [page, changePage] = useState('tags');

    return (
        <div className={styles.SearchSideBar}>
            <TextInput placeholder="Search" />
            <div className={styles.Buttons}>
                <IconButton icon="tag" onClick={() => changePage('tags')} />
                <IconButton icon="gears" onClick={() => changePage('settings')} />
            </div>

            {
                page === 'tags'     ? <TagsPanel {...props}/> :
                page === 'settings' ? <SettingsPanel {...props}/> :
                null
            }
        </div>
    );
}

// -----------------------------------------------------------------------------

const TagsPanel = (props: Props) => (
    <div className={styles.Panel}>
        <h1>Taglist</h1>
        <div className={styles.TagList}>
            {
                !props.tags ? null : props.tags.map(tag => (
                    <Tag tag={tag}/>
                ))
            }
            <Tag tag="yoshimitsu" category="artist" />
            <Tag tag="fighter" />
        </div>
    </div>
);

const SettingsPanel = (props: Props) => (
    <div className={styles.Panel}>
        <h1>Search Settings</h1>

        <Toggle on text="Hide NSFW" onPress={() => {}} />
        <Toggle on text="Hide Favorites" onPress={() => {}} />
        <Toggle on text="Only Untagged" onPress={() => {}} />
    </div>
);

export default SearchSideBar;
