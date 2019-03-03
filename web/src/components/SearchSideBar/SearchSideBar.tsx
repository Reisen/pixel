import React, { useState } from 'react';
import styles              from './SearchSideBar.module.css';

import IconButton          from '../IconButton';
import SettingsPanel       from './panels/SettingsPanel';
import TagsPanel           from './panels/TagPanel';
import TextInput           from '../TextInput';
import Toggle              from '../Toggle';


interface Props {
    tags?: string[];
}

const SearchSideBar = (props: Props) => {
    const [page, changePage] = useState('tags');

    return (
        <div className={styles.Root}>
            <TextInput placeholder="Search" />
            <div className={styles.Buttons}>
                <IconButton tooltip="Tag List" icon="tag" onClick={() => changePage('tags')} />
                <IconButton tooltip="Settings" icon="gears" onClick={() => changePage('settings')} />
            </div>

            {
                page === 'tags'     ? <TagsPanel {...props}/> :
                page === 'settings' ? <SettingsPanel /> :
                null
            }
        </div>
    );
}

export default SearchSideBar;
