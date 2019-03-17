import React, { useState } from 'react';
import styles              from './SearchSideBar.module.css';

import IconButton          from '../IconButton';
import MetaDataPanel       from './panels/MetaDataPanel';
import SettingsPanel       from './panels/SettingsPanel';
import TagsPanel           from './panels/TagPanel';
import TextInput           from '../TextInput';
import Toggle              from '../Toggle';


interface Props {
    tags?: string[];
}

const SearchSideBar = (props: Props) => {
    const [page, changePage] = useState('metadata');

    return (
        <div className={styles.Root}>
            <TextInput placeholder="Search" />
            <div className={styles.Buttons}>
                <IconButton
                    icon="tag"
                    tooltip="Tag List"
                    active={page === 'tag'}
                    onClick={() => changePage('tags')}
                />

                <IconButton
                    icon="gears"
                    tooltip="Settings"
                    active={page === 'settings'}
                    onClick={() => changePage('settings')}
                />

                <IconButton
                    icon="chart-radar-graph"
                    tooltip="Metadata"
                    active={page === 'metadata'}
                    onClick={() => changePage('metadata')}
                />
            </div>

            {
                page === 'tags'     ? <TagsPanel {...props}/> :
                page === 'settings' ? <SettingsPanel /> :
                page === 'metadata' ? <MetaDataPanel /> :
                null
            }
        </div>
    );
}

export default SearchSideBar;
